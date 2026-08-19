{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Pattern language for the rule DSL, the interpreted rule types and the
--   full rule table ("Tables"). Context-aware matching resolves the special
--   selectors (~я~, ~родители~, ~чистый~, ~гибрид~, ~доминирующий~,
--   ~смешанные~) against the acting chibik and the current field state.
module Pattern where

import           Control.Lens

import qualified Data.Map    as M
import qualified Data.Text   as T

import           Species

import           System.Random (StdGen)

-- | A species-level token in a rule pattern.
data PatSp a =
      Any                       -- ^ * — any species
    | Var a                     -- ^ a variable (binds to a species)
    | ConstName T.Text          -- ^ a concrete species name (base or hybrid)
    | SelfPat                   -- ^ ~я~ — the acting chibik's own species
    | ParentsPat                -- ^ ~родители~ — parent colors of a hybrid
    | PurePat                   -- ^ ~чистый~ — any pure species
    | HybridPat                 -- ^ ~гибрид~ — any hybrid species
    | DominantPat               -- ^ ~доминирующий~ — the dominant species
    | CrossPat                  -- ^ ~смешанные~ — just reproduced cross-color
  deriving Eq

instance Show a => Show (PatSp a) where
    show Any           = "*"
    show (Var v)       = show v
    show (ConstName c) = T.unpack c
    show SelfPat       = "~я~"
    show ParentsPat    = "~родители~"
    show PurePat       = "~чистый~"
    show HybridPat     = "~гибрид~"
    show DominantPat   = "~доминирующий~"
    show CrossPat      = "~смешанные~"

-- | A pattern: any species, a single species, or a hybrid of two species.
data Pat a = PAny | P1 (PatSp a) | P2 (PatSp a) (PatSp a) deriving Eq

instance Show a => Show (Pat a) where
    show PAny       = "*"
    show (P1 p)     = show p
    show (P2 p0 p1) = "(" ++ show p0 ++ ", " ++ show p1 ++ ")"

-- | Context for matching patterns against species.
data MatchCtx b = MatchCtx {
      _mcActor    :: Species b,                 -- the acting chibik
      _mcSpace    :: Space b,                   -- current field state
      _mcDominant :: Maybe (Species b),         -- dominant species, tie broken
      _mcResolve  :: T.Text -> Maybe (Species b) -- name resolver
    }

-- | Match a token against a species; variables bind to whole species values.
tokenMatch :: Ord b => MatchCtx b -> PatSp b -> Species b -> Maybe (M.Map b (Species b))
tokenMatch _ Any _ = Just M.empty
tokenMatch _ (Var x) s = Just (M.singleton x s)
tokenMatch ctx (ConstName t) s = do
    r <- _mcResolve ctx t
    if r == s then Just M.empty else Nothing
tokenMatch ctx SelfPat s = if s == _mcActor ctx then Just M.empty else Nothing
tokenMatch ctx ParentsPat s =
    case _mcActor ctx of
      Mix a b | s == Pure a || s == Pure b -> Just M.empty
      _ -> Nothing
tokenMatch _ PurePat s = if isPure s then Just M.empty else Nothing
tokenMatch _ HybridPat s = if isHybrid s then Just M.empty else Nothing
tokenMatch ctx DominantPat s = if Just s == _mcDominant ctx then Just M.empty else Nothing
tokenMatch ctx CrossPat s =
    if M.findWithDefault 0 s (_cross (_mcSpace ctx)) > 0 then Just M.empty else Nothing

-- | Combine two substitutions, requiring consistency on shared variables.
mergeSubst :: Ord b => M.Map b (Species b) -> M.Map b (Species b) -> Maybe (M.Map b (Species b))
mergeSubst m0 m1 =
    let common = M.intersectionWith (==) m0 m1
    in if M.null common || and (M.elems common)
       then Just (M.union m0 m1)
       else Nothing

-- | Match a pattern against a species. P1 matches any single species
--   (pure or hybrid); P2 matches hybrids only.
patMatch :: Ord b => MatchCtx b -> Pat b -> Species b -> Maybe (M.Map b (Species b))
patMatch _ PAny _ = Just M.empty
patMatch ctx (P1 t) s = tokenMatch ctx t s
patMatch ctx (P2 t0 t1) (Mix a b) = do
    m0 <- tokenMatch ctx t0 (Pure a)
    m1 <- tokenMatch ctx t1 (Pure b)
    mergeSubst m0 m1
patMatch _ (P2 _ _) (Pure _) = Nothing

-- | Concrete-ness of a single token (used to order rules: most specific first).
tokenConcr :: PatSp a -> Int
tokenConcr Any           = 0
tokenConcr (Var _)       = 0
tokenConcr (ConstName _) = 1
tokenConcr _             = 1

-- | Concrete-ness of a pattern.
concreteness :: Pat a -> Int
concreteness PAny       = 0
concreteness (P1 t)     = tokenConcr t
concreteness (P2 t0 t1) = tokenConcr t0 + tokenConcr t1

-- | All species names referenced by a pattern (for validation).
tokNames :: PatSp a -> [T.Text]
tokNames (ConstName c) = [c]
tokNames _             = []

patternNames :: Pat a -> [T.Text]
patternNames PAny       = []
patternNames (P1 t)     = tokNames t
patternNames (P2 t0 t1) = tokNames t0 ++ tokNames t1

-- | Probability that a species chooses Kill instead of Fuck when acting.
data ActionRule b =
      ActionKillProb Rational    -- ^ kill with probability p, reproduce with 1-p
    | ActionKillCross            -- ^ white: kill iff a cross-color reproducer exists
    | ActionKillCrossProb Rational -- ^ kill cross-color reproducers with prob p
  deriving Show

-- | Partner preference when initiating reproduction: a preferred set and a
--   fallback set used when no preferred partner is available. The optional
--   condition gates the fallback (e.g. white: only initiate with other pure
--   colors when few whites remain).
data PartnerRule b = PartnerRule {
      _prPreferred :: [PatSp b],
      _prFallback  :: [PatSp b],
      _prCondition :: Maybe Cond
    } deriving Show

-- | Kill target rule: species matching any target token, minus exclusions.
data KillRule b = KillRule {
      _krTargets :: [PatSp b],
      _krExclude :: [Species b]
    } deriving Show

-- | A state condition on a rule clause, e.g. [Белый < 5].
data Cond = CondLess T.Text Int | CondLeq T.Text Int
          | CondGreater T.Text Int | CondGeq T.Text Int
  deriving Show

-- | Evaluate a condition against the current field state.
evalCond :: Tables T.Text -> Space T.Text -> Cond -> Bool
evalCond tbl sp c =
    case c of
      CondLess n k    -> count n < k
      CondLeq n k     -> count n <= k
      CondGreater n k -> count n > k
      CondGeq n k     -> count n >= k
  where
    count name =
        let s = M.findWithDefault (Pure name) name (_nameResolver tbl)
        in M.findWithDefault 0 s (_population sp)

-- | Sympathy (agreement) rule: an actor pattern and an ordered list of
--   clauses. The first clause whose partner pattern matches the partner
--   species (with consistent variable bindings) and whose condition holds
--   gives the agreement probability.
data SympathyRule b = SympathyRule {
      _srActor   :: Pat b,
      _srClauses :: [(Pat b, Rational, Maybe Cond)]
    } deriving Show

-- | Creation rule: actor pattern, partner pattern and result clauses.
data CreationRule b = CreationRule {
      _crActor   :: Pat b,
      _crPartner :: Pat b,
      _crResults :: [(Pat b, Rational)]
    } deriving Show

-- | The full interpreted rule set, environment, random generator and the
--   current field state.
data Tables b = Tables {
      _randGen       :: StdGen,
      _spBase        :: [b],
      _spAll         :: [Species b],
      _env           :: Env,
      _space         :: Space b,
      _nameResolver  :: M.Map T.Text (Species b),
      _spName        :: M.Map (Species b) T.Text,
      _spColor       :: M.Map (Species b) T.Text,
      _actionRules   :: M.Map (Species b) (ActionRule b),
      _partnerRules  :: M.Map (Species b) (PartnerRule b),
      _killRules     :: M.Map (Species b) (KillRule b),
      _sympathyRules :: [SympathyRule b],
      _creationRules :: [CreationRule b]
    }
makeLenses ''Tables

-- | Name of a species in the current rule set.
speciesName :: Tables T.Text -> Species T.Text -> T.Text
speciesName tbl s = M.findWithDefault (autoName s) s (_spName tbl)
  where
    autoName (Pure x)  = x
    autoName (Mix x y) = x <> "+" <> y
