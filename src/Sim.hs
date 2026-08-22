{-# LANGUAGE OverloadedStrings #-}

-- | The game simulation: turn logic, action resolution, partner and victim
--   selection, agreement (sympathy) evaluation, creation outcomes, the
--   finite-lifespan mechanics (aging, kill success, offspring distributions,
--   lifespan histograms) and the end-of-game conditions. All randomness goes
--   through the @_randGen@ lens of "Pattern.Tables". Species names are
--   'T.Text'. The population is tracked per individual: every chibik carries
--   its age and its current probability of dying at the end of the turn.
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
        mx = maximum (0 : map length (M.elems pop))
    if mx <= 0 then return Nothing
    else sampleUniform [s | (s, ls) <- M.toList pop, length ls == mx]

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

-- | Outcome of an attempted action.
data ActionResult =
      ARDone         -- ^ the action was performed
    | ARImpossible   -- ^ the action could not be attempted (no victim/partner,
                     --   field full): the other action may be tried
    | ARMissed       -- ^ a kill was attempted but failed (the kill success
                     --   roll): the turn is spent, no fallback
  deriving (Eq, Show)

-- | Pick a random individual of the species that will act (its index in the
--   species' life list). Only individuals that have not acted yet this turn
--   are eligible (each chibik acts exactly once per turn, 2.6); the picked
--   individual is marked as acted before the action starts, so the index is
--   valid regardless of later list mutations.
pickActor :: Species T.Text -> SimCtx (Maybe Int)
pickActor a = do
    tbl <- get
    let ls = M.findWithDefault [] a (view (space.population) tbl)
        idxs = [ i | (i, l) <- zip [0 ..] ls, not (_lifeActed l) ]
    case idxs of
      [] -> return Nothing
      _  -> do
          let d = fromDistribution (uniform idxs)
          i <- sample randGen d
          modify (over (space.population) (M.adjust (updateAt i (set lifeActed True)) a))
          return (Just i)

-- | Probability that a kill attempt of the species succeeds (Успех убийства).
killSuccessProb :: Species T.Text -> SimCtx Rational
killSuccessProb a = gets (M.findWithDefault 1 a . view killSuccess)

-- | Kill reward (Награда за убийство): a successful kill decreases the
--   killer's probability of dying (down to zero).
applyKillReward :: Species T.Text -> Int -> SimCtx ()
applyKillReward a i = do
    r <- gets (view (env.envKillReward))
    modify (over (space.population) $
        M.adjust (updateAt i (over lifeDeath (max 0 . subtract r))) a)

-- | Try to perform the given action for the acting chibik (individual @i@ of
--   species @a@).
tryAction :: Species T.Text -> Int -> Action -> SimCtx ActionResult
tryAction a i Kill = do
    mv <- chooseVictim a
    case mv of
      Nothing -> return ARImpossible
      Just v  -> do
          ok <- sampleBool =<< killSuccessProb a
          if ok
          then do
              -- reward first, while the actor's index is still valid: killing
              -- own species would shift the list and misapply the reward
              applyKillReward a i
              killVictim a i v
              return ARDone
          else return ARMissed
tryAction a i Fuck = do
    tbl <- get
    if view (space.freeSpace) tbl <= 0 then return ARImpossible
    else do
      mp <- choosePartner a
      case mp of
        Nothing -> return ARImpossible
        Just p  -> do
            fuckFlow a i p
            return ARDone

-- | Perform one action for a chibik of the given species (preferred action
--   first, falling back to the other one when impossible).
actSpecies :: Species T.Text -> SimCtx ()
actSpecies a = do
    mact <- pickActor a
    case mact of
      Nothing -> return ()
      Just i  -> do
          pref <- decideAction a
          res <- tryAction a i pref
          case res of
            ARDone       -> return ()
            ARMissed     -> return ()
            ARImpossible -> do
                _ <- tryAction a i (if pref == Kill then Fuck else Kill)
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
                length (M.findWithDefault [] s pop) - (if s == a then 1 else 0)
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
            let popN = length (M.findWithDefault [] s (view population sp))
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

-- | The full reproduction flow: agreement, then creation or rape. The acting
--   chibik is the @i@-th individual of species @a@; it is the one that gets
--   the reproduction penalty, and the one the partner kills on refusal.
fuckFlow :: Species T.Text -> Int -> Species T.Text -> SimCtx ()
fuckFlow a i p = do
    agree <- sympathyProb a p
    agreed <- sampleBool agree
    if agreed then doRepro a i p
    else do
        tbl <- get
        rape <- sampleBool (view (env.envRape) tbl)
        if rape then doRepro a i p
        else killAt a i   -- the partner kills the initiator (that individual)

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

-- | Birth of one child of a x p (a free slot is assumed to exist). The child
--   starts with age 0 and the species' starting death probability.
createChild :: Species T.Text -> Species T.Text -> SimCtx (Species T.Text)
createChild a p = do
    child <- creationResult a p
    tbl <- get
    let p0 = M.findWithDefault (1 % 100) child (view lifeStart tbl)
    modify (over (space.population) (M.adjust (++ [Life 0 p0 False]) child))
    modify (over (space.freeSpace) (subtract 1))
    return child

-- | A successful reproduction event: draw the number of offspring from the
--   acting species' distribution (Потомство), create the children (as many
--   as free slots allow), block the parents and all children, mark
--   cross-color reproducers and apply the reproduction penalty to the acting
--   chibik (@i@-th individual of species @a@). A drawn zero produces no
--   offspring: nothing happens, no penalty. The blocked/cross counters are
--   clamped to the species' populations so the invariant
--   B(s) ≤ N(s), C(s) ≤ N(s) (11.2) holds at all times.
doRepro :: Species T.Text -> Int -> Species T.Text -> SimCtx ()
doRepro a i p = do
    tbl <- get
    let dist = M.findWithDefault [(1, 1)] a (view offspring tbl)
    n <- sample randGen (fromDistribution (fromList dist))
    if n <= 0 then return ()
    else do
        kids <- createChildren n
        tbl' <- get
        let popN s = length (M.findWithDefault [] s (view (space.population) tbl'))
            bump key m = M.insertWith (+) key 1 m
            clampTo key m = M.adjust (\b -> min b (popN key)) key m
            affected = a : p : kids
            mBlkInc = foldr bump M.empty affected
            mCrsInc = if a /= p then foldr bump M.empty [a, p] else M.empty
        -- rule 2: parents and all children are blocked for the rest of the
        -- turn; insertWith re-creates the entries every turn (finishTurn
        -- clears the maps; M.adjust would silently do nothing on the
        -- cleared maps from the second turn on). The counters are clamped
        -- to the species' populations after the merge, so the invariant
        -- B(s) ≤ N(s), C(s) ≤ N(s) (11.2) holds at all times.
        modify (over (space.blocked) $ \m ->
            foldr clampTo (M.unionWith (+) mBlkInc m) affected)
        -- cross-color reproducers (targets of the White behavior)
        when (a /= p) $
            modify (over (space.cross) $ \m ->
                foldr clampTo (M.unionWith (+) mCrsInc m) [a, p])
        -- reproduction penalty (Штраф за размножение) on the acting chibik
        c <- gets (view (env.envReproPenalty))
        modify (over (space.population) $
            M.adjust (updateAt i (over lifeDeath (min 1 . (+ c)))) a)
  where
    -- children are appended at the end of the lists, so the actor's index
    -- stays valid while they are created
    createChildren k = do
        tbl' <- get
        if view (space.freeSpace) tbl' <= 0 || k <= 0 then return []
        else do
            child <- createChild a p
            rest <- createChildren (k - 1)
            return (child : rest)

-- | Kill a random chibik of the victim species — never the acting chibik
--   (individual @i@ of species @a@, excluded when it belongs to the victim
--   species).
killVictim :: Species T.Text -> Int -> Species T.Text -> SimCtx ()
killVictim a i v = do
    tbl <- get
    let ls = M.findWithDefault [] v (view (space.population) tbl)
        idxs = [ j | j <- [0 .. length ls - 1], v /= a || j /= i ]
    case idxs of
      [] -> return ()
      _  -> do
          let d = fromDistribution (uniform idxs)
          j <- sample randGen d
          killAt v j

-- | Kill the i-th chibik of the given species: remove it, free a slot and
--   record its age at death in the lifespan histogram.
killAt :: Species T.Text -> Int -> SimCtx ()
killAt v i = do
    tbl <- get
    let ls = M.findWithDefault [] v (view (space.population) tbl)
    case drop i ls of
      [] -> return ()
      (l : _) -> do
          let ls' = deleteAt i ls
              newN = length ls'
          modify (over (space.population) (M.insert v ls'))
          modify (over (space.freeSpace) (+ 1))
          -- a blocked/cross chibik that dies no longer counts
          modify (over (space.blocked) (M.adjust (\b -> min b newN) v))
          modify (over (space.cross)   (M.adjust (\c -> min c newN) v))
          recordDeath v (_lifeAge l)

-- | Record an age at death in the lifespan histogram of a species.
recordDeath :: Species T.Text -> Int -> SimCtx ()
recordDeath s age = do
    tbl <- get
    let h = view (space.histogram) tbl
        addAge (Just m) = Just (M.insertWith (+) age 1 m)
        addAge Nothing  = Just (M.singleton age 1)
    modify (set (space.histogram) (M.alter addAge s h))

-- | End of turn: each chibik dies with its current death probability; the
--   survivors age by one turn and their death probability rises by 1/L per
--   the species' lifespan scale (the aging law). Deaths free slots and are
--   recorded in the lifespan histogram.
ageAndDie :: SimCtx ()
ageAndDie = do
    tbl <- get
    let spp = M.keys (view (space.population) tbl)
    forM_ spp $ \s -> do
        ls <- gets (M.findWithDefault [] s . view (space.population))
        scale <- gets (M.findWithDefault 60 s . view lifeScale)
        (ls', freed) <- foldM (stepLife s scale) ([], 0) ls
        modify (over (space.population) (M.insert s (reverse ls')))
        when (freed > 0) $
            modify (over (space.freeSpace) (+ freed))
  where
    stepLife s scale (acc, freed) l = do
        die <- sampleBool (_lifeDeath l)
        if die
        then do
            recordDeath s (_lifeAge l)
            return (acc, freed + 1)
        else do
            let inc = 1 % fromIntegral scale
                l' = l & lifeAge   +~ 1
                       & lifeDeath %~ min 1 . (+ inc)
            return (l' : acc, freed)

-- | End-of-game check. Returns the result description when the game is over.
checkEnd :: SimCtx (Maybe T.Text)
checkEnd = do
    tbl <- get
    let sp = view space tbl
        e = view env tbl
        free = view freeSpace sp
        pop = view population sp
        total = sum (map length (M.elems pop))
    if free <= 0
    then return (Just "Поле полностью заполнено")
    else if total < 5
    then return (Just "Вымирание: на поле осталось менее 5 чибиков")
    else
        case [ s | (s, ls) <- M.toList pop, length ls >= view envWin e ] of
          (s : _) -> return (Just ("Победа вида: " <> speciesName tbl s))
          [] -> return Nothing

-- | Perform one full turn: every chibik alive at the start of the turn acts
--   once, in random order; at the end of the turn the lifespan mechanics
--   run (aging and death). Returns the end-of-game result (Nothing when the
--   game continues).
stepTurn :: SimCtx (Maybe T.Text)
stepTurn = do
    -- each turn starts with every individual unacted (2.6: acts once)
    modify (over (space.population) (M.map (map (set lifeActed False))))
    tbl <- get
    let remaining = fmap length (view (space.population) tbl)
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
                                    , not (null (M.findWithDefault [] s pop)) ]
            case eligible of
              [] -> finishTurn Nothing
              _ -> do
                ms <- sampleWeighted eligible
                case ms of
                  Nothing -> finishTurn Nothing
                  Just s -> do
                    actSpecies s
                    go (M.adjust pred s remaining')

    finishTurn mres = case mres of
        Just r -> do
            -- the game is over: no aging; blocked/cross state is reset
            modify (set (space.blocked) M.empty . set (space.cross) M.empty)
            return (Just r)
        Nothing -> do
            -- end of turn: aging and death, then the end conditions are
            -- re-checked (old age can cause extinction)
            ageAndDie
            modify (set (space.blocked) M.empty . set (space.cross) M.empty)
            checkEnd

-- | The initial field for a rule set: @Начало@ chibiks of each base species,
--   all with age 0 and the species' starting death probability.
initialSpace :: Tables T.Text -> Space T.Text
initialSpace tbl =
    let e = view env tbl
        bases = view spBase tbl
        allSp = view spAll tbl
        pop0 = M.fromList [(s, []) | s <- allSp]
        life0 b = Life 0 (M.findWithDefault (1 % 100) (Pure b) (view lifeStart tbl)) False
        pop = foldr (\b -> M.insert (Pure b) (replicate (view envInitial e) (life0 b))) pop0 bases
        total = length bases * view envInitial e
        free = view envSpaceSize e - total
        zeros = M.fromList [(s, 0) | s <- allSp]
        hist0 = M.fromList [(s, M.empty) | s <- allSp]
    in Space free pop zeros zeros hist0

-- ---------------------------------------------------------------------------
-- Small list helpers

-- | Apply a function to the i-th element of a list (no-op when the index is
--   outside the list).
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i _ xs | i < 0 = xs
updateAt i f xs = case splitAt i xs of
    (l0, x : post) -> l0 ++ f x : post
    _ -> xs

-- | Remove the i-th element of a list (no-op when the index is outside the
--   list).
deleteAt :: Int -> [a] -> [a]
deleteAt i xs | i < 0 = xs
deleteAt i xs = case splitAt i xs of
    (l0, _ : post) -> l0 ++ post
    _ -> xs
