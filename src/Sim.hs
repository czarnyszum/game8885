{-# LANGUAGE OverloadedStrings #-}

-- | The game simulation: turn logic, action resolution, partner and victim
--   selection, agreement (sympathy) evaluation, creation outcomes and the
--   end-of-game conditions. All randomness goes through the @_randGen@ lens
--   of "Pattern.Tables". Species names are 'T.Text'.
module Sim where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State

import qualified Data.Map    as M
import           Data.Ratio  ((%))
import qualified Data.Text   as T

import           Pattern
import           Species

import           Probability.Distribution (fromList, size, uniform, withProbability)
import           Probability.Sample

-- | Simulation monad: state is the full rule tables (including the random
--   generator and the current field state).
type SimCtx a = StateT (Tables T.Text) IO a

-- | Run a simulation action from a given tables value.
runSim :: Tables T.Text -> SimCtx a -> IO (a, Tables T.Text)
runSim tbl act = runStateT act tbl

-- | Sample a boolean with the given probability.
sampleBool :: Rational -> SimCtx Bool
sampleBool p = sample randGen (fromDistribution (withProbability p))

-- | Uniform choice among a list of species.
sampleUniform :: [Species T.Text] -> SimCtx (Maybe (Species T.Text))
sampleUniform [] = return Nothing
sampleUniform xs = do
    let d = fromDistribution (uniform xs)
    s <- sample randGen d
    return (Just s)

-- | Population-weighted choice among (species, weight) pairs.
sampleWeighted :: [(Species T.Text, Int)] -> SimCtx (Maybe (Species T.Text))
sampleWeighted xs = do
    let d0 = fromList [(s, fromIntegral n :: Rational) | (s, n) <- xs, n > 0]
    if size d0 == 0 then return Nothing
    else do
        s <- sample randGen (fromDistribution d0)
        return (Just s)

-- | The dominant species (the one with the maximum population); ties are
--   broken uniformly at random.
dominantSpecies :: SimCtx (Maybe (Species T.Text))
dominantSpecies = do
    tbl <- get
    let pop = view (space.population) tbl
        mx = maximum (0 : M.elems pop)
    if mx <= 0 then return Nothing
    else sampleUniform [s | (s, n) <- M.toList pop, n == mx]

-- | Name resolver used inside pattern contexts.
resolve :: Tables T.Text -> T.Text -> Maybe (Species T.Text)
resolve tbl = (`M.lookup` view nameResolver tbl)

-- | Current match context for the acting species.
matchCtx :: Species T.Text -> Maybe (Species T.Text) -> SimCtx (MatchCtx T.Text)
matchCtx a mdom = do
    tbl <- get
    let sp = view space tbl
    return (MatchCtx a sp mdom (resolve tbl))

-- | Decide which action the acting chibik prefers.
decideAction :: Species T.Text -> SimCtx Action
decideAction a = do
    tbl <- get
    case M.findWithDefault (ActionKillProb 0) a (view actionRules tbl) of
      ActionKillProb p -> do
          k <- sampleBool p
          return (if k then Kill else Fuck)
      ActionKillCrossProb p -> do
          hasTarget <- hasCrossTarget a
          k <- sampleBool p
          return (if hasTarget && k then Kill else Fuck)
      ActionKillCross -> do
          hasTarget <- hasCrossTarget a
          return (if hasTarget then Kill else Fuck)

-- | Does a cross-color reproducer of another species exist?
hasCrossTarget :: Species T.Text -> SimCtx Bool
hasCrossTarget a = do
    sp <- gets (view space)
    let crossMap = view cross sp
    return (any (\s -> s /= a && M.findWithDefault 0 s crossMap > 0) (M.keys crossMap))

-- | Try to perform the given action. Returns False when the action is not
--   possible (no victim / no partner / field full).
tryAction :: Species T.Text -> Action -> SimCtx Bool
tryAction a Kill = do
    mv <- chooseVictim a
    case mv of
      Nothing -> return False
      Just v  -> killOne v >> return True
tryAction a Fuck = do
    tbl <- get
    if view (space.freeSpace) tbl <= 0 then return False
    else do
      mp <- choosePartner a
      case mp of
        Nothing -> return False
        Just p  -> fuckFlow a p >> return True

-- | Perform one action for a chibik of the given species (preferred action
--   first, falling back to the other one when impossible).
actSpecies :: Species T.Text -> SimCtx ()
actSpecies a = do
    pref <- decideAction a
    ok <- tryAction a pref
    unless ok $ do
        _ <- tryAction a (if pref == Kill then Fuck else Kill)
        return ()

-- | Choose a kill victim for the acting species, per its kill rule.
chooseVictim :: Species T.Text -> SimCtx (Maybe (Species T.Text))
chooseVictim a = do
    tbl <- get
    let sp = view space tbl
        pop = view population sp
        allSp = view spAll tbl
    case M.lookup a (view killRules tbl) of
      Nothing -> return Nothing
      Just (KillRule targets excl) -> do
        mdom <- dominantSpecies
        ctx <- matchCtx a mdom
        let victimCount s =
                M.findWithDefault 0 s pop - (if s == a then 1 else 0)
            eligible =
              [ s | s <- allSp
                  , victimCount s > 0
                  , s `notElem` excl
                  , any (\t -> tokenMatch ctx t s /= Nothing) targets
                  ]
        case eligible of
          [] -> return Nothing
          _  -> sampleWeighted [(s, victimCount s) | s <- eligible]

-- | Choose a partner for reproduction, per the acting species' partner rule.
choosePartner :: Species T.Text -> SimCtx (Maybe (Species T.Text))
choosePartner a = do
    tbl <- get
    let sp = view space tbl
        allSp = view spAll tbl
        rule = M.findWithDefault (PartnerRule [Any] [] Nothing) a (view partnerRules tbl)
        actBlocked = M.findWithDefault 0 a (view blocked sp) > 0
    mdom <- dominantSpecies
    ctx <- matchCtx a mdom
    let partnerCount s =
            let popN = M.findWithDefault 0 s (view population sp)
                blkN = M.findWithDefault 0 s (view blocked sp)
                unblocked = max 0 (popN - blkN)
            in unblocked - (if s == a && not actBlocked then 1 else 0)
        pick tokSet = [ s | s <- allSp
                       , partnerCount s > 0
                       , any (\t -> tokenMatch ctx t s /= Nothing) tokSet ]
    let preferred = pick (_prPreferred rule)
    case preferred of
      (_ : _) -> sampleUniform preferred
      [] -> do
          let gated = case _prCondition rule of
                        Nothing -> True
                        Just c  -> evalCond tbl sp c
          if not gated then return Nothing
          else do
              let fallback = pick (_prFallback rule)
              case fallback of
                [] -> return Nothing
                _  -> sampleUniform fallback

-- | The full reproduction flow: agreement, then creation or rape.
fuckFlow :: Species T.Text -> Species T.Text -> SimCtx ()
fuckFlow a p = do
    agree <- sympathyProb a p
    agreed <- sampleBool agree
    if agreed then creation a p
    else do
        tbl <- get
        rape <- sampleBool (view (env.envRape) tbl)
        if rape then creation a p
        else killOne a   -- the partner kills the initiator

-- | Agreement probability of the partner towards the acting chibik.
sympathyProb :: Species T.Text -> Species T.Text -> SimCtx Rational
sympathyProb a p
    | a == p = return 1   -- rule 3: own species always agrees
    | otherwise = do
        tbl <- get
        let sp = view space tbl
            rules = view sympathyRules tbl
            ctx = MatchCtx a sp Nothing (resolve tbl)
            firstClause _ [] = Nothing
            firstClause subst0 ((ppat, prob, mcond) : cs) =
                let condOk = case mcond of
                               Nothing -> True
                               Just c  -> evalCond tbl sp c
                in if not condOk then firstClause subst0 cs
                   else case patMatch ctx ppat p of
                          Nothing -> firstClause subst0 cs
                          Just subst1 ->
                              case mergeSubst subst0 subst1 of
                                Just _  -> Just prob
                                Nothing -> firstClause subst0 cs
            go [] = return (1 % 2)   -- default: neutral 50/50
            go (SympathyRule apat clauses : rest) =
                case patMatch ctx apat a of
                  Nothing -> go rest
                  Just subst0 ->
                      case firstClause subst0 clauses of
                        Just prob -> return prob
                        Nothing   -> go rest
        go rules

-- | Resolve a creation result pattern to a species.
resolveResult :: M.Map T.Text (Species T.Text) -> Pat T.Text -> Tables T.Text
              -> Maybe (Species T.Text)
resolveResult subst rpat tbl =
    case rpat of
      P1 t -> resolveTok subst t
      P2 t0 t1 -> do
          s0 <- resolveTok subst t0
          s1 <- resolveTok subst t1
          combine s0 s1
      PAny -> Nothing
  where
    resolveTok _ Any = Nothing
    resolveTok subst' (Var x) = M.lookup x subst'
    resolveTok _ (ConstName t) = resolve tbl t
    resolveTok _ _ = Nothing
    combine (Pure x) (Pure y) | x /= y = Just (mix x y)
    combine _ _ = Nothing

-- | Child species for a reproduction of a with p.
creationResult :: Species T.Text -> Species T.Text -> SimCtx (Species T.Text)
creationResult a p
    | a == p = return a   -- own species always produce own
    | otherwise = do
        tbl <- get
        let sp = view space tbl
            rules = view creationRules tbl
            ctx = MatchCtx a sp Nothing (resolve tbl)
            -- first matching rule (most specific first), with its substitution
            firstMatch [] = Nothing
            firstMatch (CreationRule apat ppat results : rest) =
                case patMatch ctx apat a of
                  Nothing -> firstMatch rest
                  Just subst0 ->
                      case patMatch ctx ppat p of
                        Nothing -> firstMatch rest
                        Just subst1 ->
                            case mergeSubst subst0 subst1 of
                              Nothing -> firstMatch rest
                              Just subst -> Just (subst, results)
            -- all result clauses that resolve to a valid species
            options subst results =
                [ (s, prob) | (rpat, prob) <- results
                            , Just s <- [resolveResult subst rpat tbl]
                            , s `elem` view spAll tbl ]
            defaultParents = do
                b <- sampleBool (1 % 2)
                return (if b then a else p)
        case firstMatch rules of
          Nothing -> defaultParents
          Just (subst, results) ->
              case options subst results of
                [] -> defaultParents
                opts -> do
                    s <- sample randGen (fromDistribution (fromList opts))
                    return s

-- | Kill a chibik of the given species (frees a slot).
killOne :: Species T.Text -> SimCtx ()
killOne v = do
    tbl <- get
    let popN = M.findWithDefault 0 v (view (space.population) tbl)
    when (popN > 0) $ do
        let newN = popN - 1
        modify (over (space.population) (M.adjust pred v))
        modify (over (space.freeSpace) (+ 1))
        -- a blocked/cross chibik that dies no longer counts
        modify (over (space.blocked) (M.adjust (\b -> min b newN) v))
        modify (over (space.cross)   (M.adjust (\c -> min c newN) v))

-- | Birth of a child (needs a free slot).
creation :: Species T.Text -> Species T.Text -> SimCtx ()
creation a p = do
    child <- creationResult a p
    modify (over (space.population) (M.adjust (+ 1) child))
    modify (over (space.freeSpace) (subtract 1))
    -- rule 2: parents and child are blocked for the rest of the turn
    modify (over (space.blocked) (M.adjust (+ 1) a . M.adjust (+ 1) p . M.adjust (+ 1) child))
    -- white's targets: parents that just reproduced with another color
    when (a /= p) $
        modify (over (space.cross) (M.adjust (+ 1) a . M.adjust (+ 1) p))

-- | End-of-game check. Returns the result description when the game is over.
checkEnd :: SimCtx (Maybe T.Text)
checkEnd = do
    tbl <- get
    let sp = view space tbl
        e = view env tbl
        free = view freeSpace sp
        pop = view population sp
        total = sum (M.elems pop)
    if free <= 0
    then return (Just "Поле полностью заполнено")
    else if total < 5
    then return (Just "Вымирание: на поле осталось менее 5 чибиков")
    else
        case [ s | (s, n) <- M.toList pop, n >= view envWin e ] of
          (s : _) -> return (Just ("Победа вида: " <> speciesName tbl s))
          [] -> return Nothing

-- | Perform one full turn: every chibik alive at the start of the turn acts
--   once, in random order. Returns the end-of-game result (Nothing when the
--   game continues).
stepTurn :: SimCtx (Maybe T.Text)
stepTurn = do
    tbl <- get
    let remaining = view (space.population) tbl
    go remaining
  where
    go remaining' = do
        res <- checkEnd
        case res of
          Just r -> finishTurn (Just r)
          Nothing -> do
            tbl <- get
            let pop = view (space.population) tbl
                eligible = [ (s, r) | (s, r) <- M.toList remaining'
                                    , r > 0
                                    , M.findWithDefault 0 s pop > 0 ]
            case eligible of
              [] -> finishTurn Nothing
              _ -> do
                ms <- sampleWeighted eligible
                case ms of
                  Nothing -> finishTurn Nothing
                  Just s -> do
                    actSpecies s
                    go (M.adjust pred s remaining')

    finishTurn mres = do
        -- end of turn: blocked/cross state is reset
        modify (set (space.blocked) M.empty . set (space.cross) M.empty)
        return mres

-- | The initial field for a rule set: @Начало@ chibiks of each base species.
initialSpace :: Tables T.Text -> Space T.Text
initialSpace tbl =
    let e = view env tbl
        bases = view spBase tbl
        allSp = view spAll tbl
        pop0 = M.fromList [(s, 0) | s <- allSp]
        pop = foldr (\b -> M.insert (Pure b) (view envInitial e)) pop0 bases
        total = length bases * view envInitial e
        free = view envSpaceSize e - total
        zeros = M.fromList [(s, 0) | s <- allSp]
    in Space free pop zeros zeros
