{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the game simulation: DSL parsing/compilation, probability
--   distributions (rule 4), rape mechanic, the finite-lifespan mechanics
--   (aging law, kill success, offspring distributions, lifespan histograms)
--   and end-of-game invariants.
module Test where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State

import qualified Data.Map    as M
import           Data.Ratio  ((%))
import qualified Data.Text   as T
import qualified Data.Text.IO as TIO

import           Decl
import           Parser
import           Pattern
import           Sim
import           Species

import           Ctx (readConfig, writeConfig)

import           System.Directory (removeFile)
import           System.Random (mkStdGen, randomRIO)

-- ---------------------------------------------------------------------------
-- tiny test framework

data TestResult = OK | Fail String
  deriving Show

check :: String -> Bool -> IO TestResult
check name ok = return (if ok then OK else Fail name)

-- ---------------------------------------------------------------------------
-- helpers

loadTables :: FilePath -> IO (Either String (Tables T.Text))
loadTables file = do
    content <- TIO.readFile file
    case parseRuleFile file content of
      Left err -> return (Left err)
      Right decls ->
          case compileDecls decls (mkStdGen 42) of
            Left e  -> return (Left (show e))
            Right t -> return (Right t)

-- | Compile an inline rule set (for focused tests).
mkTables :: T.Text -> IO (Either String (Tables T.Text))
mkTables src = do
    case parseRuleFile "<inline>" src of
      Left err -> return (Left err)
      Right decls ->
          case compileDecls decls (mkStdGen 42) of
            Left e  -> return (Left (show e))
            Right t -> return (Right t)

spCount :: Species T.Text -> Tables T.Text -> Int
spCount s tbl = length (M.findWithDefault [] s (view (space.population) tbl))

totalCount :: Tables T.Text -> Int
totalCount tbl = sum (map length (M.elems (view (space.population) tbl)))

histSum :: Species T.Text -> Tables T.Text -> Int
histSum s tbl = sum (M.elems (M.findWithDefault M.empty s (view (space.histogram) tbl)))

-- | Run the game to completion, checking invariants after every step.
--   Returns (result, number of steps, final tables).
runToEnd :: Tables T.Text -> Int -> IO (Maybe T.Text, Int, Tables T.Text)
runToEnd tbl0 maxSteps = go 0 tbl0
  where
    go n tbl
        | n >= maxSteps = return (Nothing, n, tbl)
        | otherwise = do
            (mres, tbl') <- runSim tbl stepTurn
            if not (checkInvariants tbl') then error ("INVARIANT VIOLATION at step " ++ show n)
            else case mres of
                   Just r  -> return (Just r, n + 1, tbl')
                   Nothing -> go (n + 1) tbl'

checkInvariants :: Tables T.Text -> Bool
checkInvariants tbl =
    let sp = view space tbl
        free = view freeSpace sp
        pop = view population sp
        blk = view blocked sp
        crs = view cross sp
        total = sum (map length (M.elems pop))
        envSize = view (env.envSpaceSize) tbl
        popOk = and [ all (\l -> _lifeAge l >= 0 && _lifeDeath l >= 0 && _lifeDeath l <= 1) ls
                    | ls <- M.elems pop ]
        blkOk = and [ M.findWithDefault 0 s blk <= length (M.findWithDefault [] s pop) | s <- M.keys blk ]
        crsOk = and [ M.findWithDefault 0 s crs <= length (M.findWithDefault [] s pop) | s <- M.keys crs ]
    in free + total == envSize && popOk && blkOk && crsOk

-- ---------------------------------------------------------------------------
-- the tests

runTests :: IO ()
runTests = do
    putStrLn "== Парсинг и компиляция правил =="
    rt <- loadTables "rules/8885.rule"
    case rt of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
        putStrLn "OK: rules/8885.rule compiled"
        let allSp = view spAll tbl0
            bases = view spBase tbl0
            resolver = view nameResolver tbl0
            sPurple = Mix "Красный" "Синий"
        putStrLn ("  базовых видов: " ++ show (length bases))
        putStrLn ("  всего видов (включая гибриды): " ++ show (length allSp))
        putStrLn ("  Фиолетовый разрешён: " ++ show (M.lookup "Фиолетовый" resolver == Just sPurple))

        -- initial field
        let tbl1 = set space (initialSpace tbl0) tbl0
            sp0 = view space tbl1
        putStrLn ("  начальное поле: свободно=" ++ show (view freeSpace sp0)
                  ++ ", красных=" ++ show (spCount sRed tbl1)
                  ++ ", всего=" ++ show (totalCount tbl1))
        check "начальное поле: 5 красных" (spCount sRed tbl1 == 5) >>= report
        check "начальное поле: 863 свободно" (view freeSpace sp0 == 888 - 25) >>= report
        check "инвариант: свободно+население=888" (checkInvariants tbl1) >>= report

        -- same-species creation
        (childSame, _) <- runSim tbl1 (creationResult sRed sRed)
        check "размножение красного с красным даёт красного" (childSame == sRed) >>= report

        -- creation probabilities (rule 4): red x blue -> 50% purple, 25/25
        (counts, _) <- runSim tbl1 (replicateM 4000 (creationResult sRed sBlue))
        let nPurple = length (filter (== sPurple) counts)
            nRed = length (filter (== sRed) counts)
            nBlue = length (filter (== sBlue) counts)
            pct4000 x = 100 * x `div` 4000
        putStrLn ("  красный x синий за 4000 опытов: фиолетовый=" ++ show (pct4000 nPurple)
                  ++ "%, красный=" ++ show (pct4000 nRed) ++ "%, синий=" ++ show (pct4000 nBlue) ++ "%")
        check "рождение: гибрид ~50%" (abs (nPurple - 2000) < 200) >>= report
        check "рождение: красный ~25%" (abs (nRed - 1000) < 150) >>= report
        check "рождение: синий ~25%" (abs (nBlue - 1000) < 150) >>= report

        -- original x hybrid: 50/50 between parents
        (counts2, _) <- runSim tbl1 (replicateM 2000 (creationResult sRed sPurple))
        let nR2 = length (filter (== sRed) counts2)
            nP2 = length (filter (== sPurple) counts2)
            pct2000 x = 100 * x `div` 2000
        putStrLn ("  красный x фиолетовый за 2000 опытов: красный=" ++ show (pct2000 nR2)
                  ++ "%, фиолетовый=" ++ show (pct2000 nP2) ++ "%")
        check "рождение: исходный x гибрид ~50/50" (abs (nR2 - 1000) < 150 && abs (nP2 - 1000) < 150) >>= report

        -- sympathy: same species 100%, red receiver 100%, white receiver 0 (>=5 whites)
        (symOwn, _) <- runSim tbl1 (sympathyProb sRed sRed)
        check "симпатия: свои всегда 100%" (symOwn == 1) >>= report
        (symRedRecv, _) <- runSim tbl1 (sympathyProb sBlue sRed)
        check "симпатия: красный принимающий всегда 100%" (symRedRecv == 1) >>= report
        (symWhiteRecv, _) <- runSim tbl1 (sympathyProb sRed sWhite)
        check "симпатия: белый принимающий 0% (белых >= 5)" (symWhiteRecv == 0) >>= report
        (symYellowRecv, _) <- runSim tbl1 (sympathyProb sBlue sYellow)
        check "симпатия: жёлтый принимающий всегда 100%" (symYellowRecv == 1) >>= report
        (symBlueRecv, _) <- runSim tbl1 (sympathyProb sRed sBlue)
        check "симпатия: синий принимающий от красного 50%" (symBlueRecv == 1 % 2) >>= report
        (symBlackRecvHyb, _) <- runSim tbl1 (sympathyProb sPurple sBlack)
        check "симпатия: чёрный принимающий от гибрида 100%" (symBlackRecvHyb == 1) >>= report
        (symBlackRecvPure, _) <- runSim tbl1 (sympathyProb sRed sBlack)
        check "симпатия: чёрный принимающий от чистого 50%" (symBlackRecvPure == 1 % 2) >>= report

        -- full games (multiple seeds)
        putStrLn "== Полные партии (8885.rule) =="
        seeds <- sequence (replicate 3 (randomRIO (1, 1000000) :: IO Int))
        results <- mapM runOneGame seeds
        mapM_ (\(seed, (mres, nsteps, _)) ->
                  putStrLn ("  seed=" ++ show seed ++ ": шагов=" ++ show nsteps
                            ++ ", результат=" ++ maybe "не завершена" T.unpack mres))
               (zip seeds results)
        let finished = all (\(r, _, _) -> maybe False (const True) r) results
        check "все партии завершились" finished >>= report

    putStrLn "== Конечная жизнь: старение (aging) =="
    agingTests

    putStrLn "== Конечная жизнь: успех убийства =="
    killSuccessTests

    putStrLn "== Конечная жизнь: потомство =="
    offspringTests

    putStrLn "== Минимальная модель triplet.rule =="
    tripletTests

    putStrLn "== Регрессии по docs/SPEC.md (блокировка, награда, условия) =="
    verificationTests

    putStrLn "== Постоянная конфигурация сервера =="
    configTests

    putStrLn "== Готово =="

sWhite :: Species T.Text
sWhite = Pure "Белый"

sYellow :: Species T.Text
sYellow = Pure "Жёлтый"

sBlue :: Species T.Text
sBlue = Pure "Синий"

sBlack :: Species T.Text
sBlack = Pure "Чёрный"

sRed :: Species T.Text
sRed = Pure "Красный"

runOneGame :: Int -> IO (Maybe T.Text, Int, Tables T.Text)
runOneGame seed = do
    rt <- loadTables "rules/8885.rule"
    case rt of
      Left err -> error err
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
              tbl2 = set randGen (mkStdGen seed) tbl1
          runToEnd tbl2 100000

-- ---------------------------------------------------------------------------
-- finite-lifespan tests

-- | Run the end-of-turn aging/death phase @n@ times.
runAging :: Tables T.Text -> Int -> IO (Tables T.Text)
runAging tbl0 n = go 0 tbl0
  where
    go k tbl | k >= n = return tbl
             | otherwise = do
                 (_, tbl') <- runSim tbl ageAndDie
                 go (k + 1) tbl'

agingTests :: IO ()
agingTests = do
    rt <- loadTables "rules/triplet.rule"
    case rt of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          -- triplet: 10 red + 10 blue, all with Смертность 2%, Долголетие 200.
          -- After enough aging rounds every chibik must be dead and every
          -- death must be recorded in the histogram.
          let tbl1 = set space (initialSpace tbl0) tbl0
              start = totalCount tbl1
          putStrLn ("  старт: " ++ show start ++ " чибиков")
          tblEnd <- runAging tbl1 600
          let left = totalCount tblEnd
              histTotal = sum [ histSum s tblEnd | s <- view spAll tbl0 ]
              ages = concatMap M.keys (M.elems (view (space.histogram) tblEnd))
          putStrLn ("  после 600 ходов старения: осталось=" ++ show left
                    ++ ", умерло (по гистограмме)=" ++ show histTotal)
          check "старение: все умерли к 600 ходам" (left == 0) >>= report
          check "старение: гистограмма учла всех умерших" (histTotal == start) >>= report
          check "старение: возрасты неотрицательны" (all (>= 0) ages) >>= report
          -- the invariant must hold throughout (free slots = deaths)
          check "инвариант после старения" (checkInvariants tblEnd) >>= report
          -- mortality at birth: after one aging round some fraction (~2%) die
          tbl1' <- runAging tbl1 1
          let died1 = start - totalCount tbl1'
          putStrLn ("  умерло за 1 ход: " ++ show died1)
          check "старение: в первый ход умирает малая доля" (died1 >= 0 && died1 <= start `div` 2) >>= report

-- | Rules with a fully deterministic kill setup: red always chooses kill;
--   kill success 0% or 100%; nobody can die from reproduction.
killTestRules :: T.Text -> T.Text
killTestRules ksuc =
    "Базовые виды: Красный, Синий;\n\
    \Параметры: { Поле: 100; Начало: 5; Победа: 60; Изнасилование: 0%; Максимум шагов: 0;\n\
    \  Награда за убийство: 5%; Штраф за размножение: 10%; }\n\
    \Действия: { Красный: 100%; Синий: 0%; }\n\
    \Убийство: { Красный: Синий; }\n\
    \Симпатии: { * < Синий: 100%; * < Красный: 100%; }\n\
    \Смертность: { Красный: 0%; Синий: 0%; *: 0%; }\n\
    \Долголетие: { Красный: 1000; Синий: 1000; *: 1000; }\n\
    \Успех убийства: { Красный: " <> ksuc <> "; Синий: 100%; *: 100%; }\n\
    \Потомство: { Красный: 0: 100%; Синий: 0: 100%; *: 0: 100%; }\n"

killSuccessTests :: IO ()
killSuccessTests = do
    rt0 <- mkTables (killTestRules "0%")
    case rt0 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (_, tbl2) <- runSim tbl1 stepTurn
          putStrLn ("  успех 0%: красных=" ++ show (spCount sRed tbl2)
                    ++ ", синих=" ++ show (spCount sBlue tbl2))
          check "успех 0%: красные не убили (мимо)" (spCount sRed tbl2 == 5 && spCount sBlue tbl2 == 5) >>= report
    rt1 <- mkTables (killTestRules "100%")
    case rt1 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (_, tbl2) <- runSim tbl1 stepTurn
          putStrLn ("  успех 100%: красных=" ++ show (spCount sRed tbl2)
                    ++ ", синих=" ++ show (spCount sBlue tbl2))
          check "успех 100%: красные убили всех синих" (spCount sRed tbl2 == 5 && spCount sBlue tbl2 == 0) >>= report
          -- the kill reward lowered the killers' death probability
          let reds = M.findWithDefault [] sRed (view (space.population) tbl2)
          check "награда: смертность убийц снижена" (all (\l -> _lifeDeath l < (1 % 100)) reds) >>= report

-- | Rules where reproduction produces a known offspring distribution of the
--   red species (the first argument) and zero offspring for everyone else.
offspringRules :: T.Text -> T.Text
offspringRules dist =
    "Базовые виды: Красный, Синий;\n\
    \Параметры: { Поле: 1000; Начало: 10; Победа: 600; Изнасилование: 0%; Максимум шагов: 0;\n\
    \  Награда за убийство: 5%; Штраф за размножение: 10%; }\n\
    \Действия: { Красный: 0%; Синий: 0%; }\n\
    \Симпатии: { * < Синий: 100%; * < Красный: 100%; }\n\
    \Смертность: { Красный: 0%; Синий: 0%; *: 0%; }\n\
    \Долголетие: { Красный: 1000; Синий: 1000; *: 1000; }\n\
    \Успех убийства: { Красный: 100%; Синий: 100%; *: 100%; }\n\
    \Потомство: { Красный: " <> dist <> "; Синий: 0: 100%; *: 0: 100%; }\n"

offspringTests :: IO ()
offspringTests = do
    rt <- mkTables (offspringRules "0: 20% 1: 60% 2: 20%")
    case rt of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          -- red x blue reproductions; each run is on a fresh field so the
          -- offspring count is never truncated by lack of free space
          (counts, _) <- runSim tbl1 (replicateM 2000 $ do
              modify (\t -> set space (initialSpace t) t)
              before <- gets totalCount
              doRepro sRed 0 sBlue
              after <- gets totalCount
              return (after - before))
          let n0 = length (filter (== 0) counts)
              n1 = length (filter (== 1) counts)
              n2 = length (filter (== 2) counts)
              mean :: Double
              mean = fromIntegral (sum counts) / 2000
              pct x = 100 * x `div` 2000
          putStrLn ("  потомство красного за 2000 опытов: 0=" ++ show (pct n0)
                    ++ "%, 1=" ++ show (pct n1) ++ "%, 2=" ++ show (pct n2)
                    ++ "%, среднее=" ++ show mean)
          check "потомство: ~20% без детей" (abs (n0 - 400) < 120) >>= report
          check "потомство: ~60% один ребёнок" (abs (n1 - 1200) < 150) >>= report
          check "потомство: ~20% двое детей" (abs (n2 - 400) < 120) >>= report
          check "потомство: среднее ~1.0" (abs (sum counts - 2000) < 300) >>= report
    -- reproduction penalty: with a deterministic one-child distribution the
    -- acting chibik's death probability must rise by Штраф за размножение
    rt1 <- mkTables (offspringRules "1: 100%")
    case rt1 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (tbl2, _) <- runSim tbl1 $ do
              modify (\t -> set space (initialSpace t) t)
              doRepro sRed 0 sBlue
              gets (\t -> t)
          let reds = M.findWithDefault [] sRed (view (space.population) tbl2)
          putStrLn ("  после размножения смертность инициатора: "
                    ++ show (case reds of (l : _) -> _lifeDeath l; [] -> -1))
          check "штраф: смертность родителя выросла на 10%"
                (case reds of (l : _) -> _lifeDeath l == 1 % 10; [] -> False) >>= report

tripletTests :: IO ()
tripletTests = do
    rt <- loadTables "rules/triplet.rule"
    case rt of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
              sPurple = Mix "Красный" "Синий"
          putStrLn "OK: rules/triplet.rule compiled"
          check "triplet: 10 красных, 10 синих" (spCount sRed tbl1 == 10 && spCount sBlue tbl1 == 10) >>= report
          check "triplet: фиолетовых нет" (spCount sPurple tbl1 == 0) >>= report
          seeds <- sequence (replicate 3 (randomRIO (1, 1000000) :: IO Int))
          results <- mapM runOneTriplet seeds
          mapM_ (\(seed, (mres, nsteps, tblEnd)) ->
                    putStrLn ("  seed=" ++ show seed ++ ": шагов=" ++ show nsteps
                              ++ ", результат=" ++ maybe "не завершена" T.unpack mres
                              ++ ", умерло всего=" ++ show (sum [ histSum s tblEnd | s <- view spAll tblEnd ])))
                 (zip seeds results)
          let finished = all (\(r, _, _) -> maybe False (const True) r) results
          check "triplet: все партии завершились" finished >>= report
          check "triplet: гистограммы не пусты" (all (\(_, _, tb) -> sum [ histSum s tb | s <- view spAll tb ] > 0) results) >>= report
          -- regression: the hybrid must actually appear during a game
          -- (the old partner preferences made red x blue pairs impossible)
          mx <- maxPurpleSeen tbl1 120
          putStrLn ("  максимум фиолетовых за 120 ходов: " ++ show mx)
          check "triplet: гибрид появляется в партии" (mx > 0) >>= report

-- | Maximum purple (hybrid) population reached during the first @n@ turns.
maxPurpleSeen :: Tables T.Text -> Int -> IO Int
maxPurpleSeen tbl0 n = go 0 (set space (initialSpace tbl0) tbl0) 0
  where
    sPurple = Mix "Красный" "Синий"
    go k tbl mx
        | k >= n = return mx
        | otherwise = do
            (mres, tbl') <- runSim tbl stepTurn
            let p = length (M.findWithDefault [] sPurple (view (space.population) tbl'))
            case mres of
              Just _  -> return (max mx p)
              Nothing -> go (k + 1) tbl' (max mx p)

runOneTriplet :: Int -> IO (Maybe T.Text, Int, Tables T.Text)
runOneTriplet seed = do
    rt <- loadTables "rules/triplet.rule"
    case rt of
      Left err -> error err
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
              tbl2 = set randGen (mkStdGen seed) tbl1
          runToEnd tbl2 100000

configTests :: IO ()
configTests = do
    -- missing config -> Nothing
    missing <- readConfig ".test_conf_missing"
    check "конфиг: нет файла -> Nothing" (missing == Nothing) >>= report
    -- write/read round-trip
    writeConfig ".test_conf" "rules/triplet.rule"
    lastRule <- readConfig ".test_conf"
    check "конфиг: round-trip" (lastRule == Just "rules/triplet.rule") >>= report
    removeFile ".test_conf"

-- ---------------------------------------------------------------------------
-- regressions found during the docs/SPEC.md verification

verificationTests :: IO ()
verificationTests = do
    -- (1) blocked/cross must accumulate after the first turn: finishTurn
    -- clears the maps, so increments must re-create entries (insertWith),
    -- not silently no-op (M.adjust) — otherwise rule-2 blocking and the
    -- White ~смешанные~ behavior die from turn 2 on.
    rt <- mkTables (offspringRules "1: 100%")
    case rt of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (_, tbl2) <- runSim tbl1 stepTurn          -- clears blocked/cross
          (_, tbl3) <- runSim tbl2 (doRepro sRed 0 sBlue)
          let blk = view (space.blocked) tbl3
              crs = view (space.cross) tbl3
          putStrLn ("  blocked после 1-го хода: красные="
                    ++ show (M.findWithDefault 0 sRed blk)
                    ++ ", синие=" ++ show (M.findWithDefault 0 sBlue blk)
                    ++ "; cross: красные=" ++ show (M.findWithDefault 0 sRed crs)
                    ++ ", синие=" ++ show (M.findWithDefault 0 sBlue crs))
          check "блокировка работает и после 1-го хода"
                (M.findWithDefault 0 sRed blk >= 1 && M.findWithDefault 0 sBlue blk >= 1) >>= report
          check "пометка «смешанных» работает и после 1-го хода"
                (M.findWithDefault 0 sRed crs >= 1 && M.findWithDefault 0 sBlue crs >= 1) >>= report
    -- (2) the kill reward must hit the ACTING chibik even when it kills its
    -- own species (a smaller-index victim would shift the list), and the
    -- acting chibik must never be the victim itself.
    rt1 <- mkTables killOwnRules
    case rt1 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let sp0 = Space 892
                    (M.fromList [ (sRed, [Life 0 (1 % 10) False, Life 0 (2 % 10) False, Life 0 (3 % 10) False])
                                , (sBlue, replicate 5 (Life 0 0 False)) ])
                    (M.fromList []) (M.fromList [])
                    (M.fromList [ (sRed, M.empty), (sBlue, M.empty) ])
              tbl1 = set space sp0 tbl0
          (_, tbl2) <- runSim tbl1 (tryAction sRed 1 Kill)
          let probs = map _lifeDeath (M.findWithDefault [] sRed (view (space.population) tbl2))
          putStrLn ("  после убийства своего вида: смерти=" ++ show probs)
          check "награда применена к убийце (индекс не сдвинулся)"
                (0.0 `elem` probs && 0.2 `notElem` probs && length probs == 2) >>= report
    -- (3) species names inside conditions must be validated at compile time
    bad1 <- mkTables ("Базовые виды: Красный, Синий;\n\
                     \Партнёры: { Красный: [Красный] -> [*] [НетТакогоВида < 5]; }\n")
    check "валидация: неизвестный вид в условии Партнёров"
          (isLeft bad1) >>= report
    bad2 <- mkTables ("Базовые виды: Красный, Синий;\n\
                     \Симпатии: { Красный < Синий: 100% [НетТакогоВида >= 3]; }\n")
    check "валидация: неизвестный вид в условии Симпатий"
          (isLeft bad2) >>= report
    -- (4) species names inside kill targets and partner tokens are validated
    bad3 <- mkTables ("Базовые виды: Красный, Синий;\n\
                     \Убийство: { Красный: НетТакогоВида; }\n")
    check "валидация: неизвестный вид в целях Убийства"
          (isLeft bad3) >>= report
    bad4 <- mkTables ("Базовые виды: Красный, Синий;\n\
                     \Партнёры: { Красный: [НетТакогоВида] -> [*]; }\n")
    check "валидация: неизвестный вид в предпочтениях Партнёров"
          (isLeft bad4) >>= report
    bad5 <- mkTables ("Базовые виды: Красный, Синий;\n\
                     \Партнёры: { Красный: [Красный] -> [НетТакогоВида]; }\n")
    check "валидация: неизвестный вид в запасных Партнёров"
          (isLeft bad5) >>= report
    -- (5) each chibik acts exactly once per turn: after all individuals of
    -- a species have acted, pickActor returns Nothing
    rt3 <- mkTables (offspringRules "1: 100%")
    case rt3 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (picks, _) <- runSim tbl1 (replicateM 11 (pickActor sRed))
          let nNothing = length (filter (== Nothing) picks)
          check "2.6: после 10 действий особей красных не осталось" (nNothing == 1) >>= report
    -- (6) blocked/cross counters stay ≤ population even after many
    -- reproductions in one turn (invariant 11.2, mid-turn)
    rt4 <- mkTables (offspringRules "1: 100%")
    case rt4 of
      Left err -> putStrLn ("FAIL: " ++ err)
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
          (_, tbl2) <- runSim tbl1 (replicateM 30 (doRepro sRed 0 sBlue))
          let sp = view space tbl2
              pop = view population sp
              blk = view blocked sp
              crs = view cross sp
              blkOk = and [ M.findWithDefault 0 s blk <= length (M.findWithDefault [] s pop)
                          | s <- M.keys blk ]
              crsOk = and [ M.findWithDefault 0 s crs <= length (M.findWithDefault [] s pop)
                          | s <- M.keys crs ]
          putStrLn ("  после 30 размножений: B(Красный)="
                    ++ show (M.findWithDefault 0 sRed blk)
                    ++ " N(Красный)=" ++ show (length (M.findWithDefault [] sRed pop)))
          check "11.2: блокировка не превышает численность" blkOk >>= report
          check "11.2: «смешанные» не превышают численность" crsOk >>= report
    -- (7) updateAt/deleteAt are no-ops for out-of-range and negative indices
    check "updateAt: отрицательный индекс — no-op"
          (updateAt (-1) (+ 1) [1, 2, 3] == [1, 2, 3]) >>= report
    check "updateAt: индекс за концом — no-op"
          (updateAt 5 (+ 1) [1, 2, 3] == [1, 2, 3]) >>= report
    check "deleteAt: отрицательный индекс — no-op"
          (deleteAt (-1) [1, 2, 3] == [1, 2, 3]) >>= report
  where
    isLeft (Left _) = True
    isLeft _        = False

-- | Rules where red always chooses kill and may kill its own species; the
--   reward is 30% so the acting chibik's death probability clamps to 0.
killOwnRules :: T.Text
killOwnRules =
    "Базовые виды: Красный, Синий;\n\
    \Параметры: { Поле: 100; Начало: 5; Победа: 60; Изнасилование: 0%; Максимум шагов: 0;\n\
    \  Награда за убийство: 30%; Штраф за размножение: 10%; }\n\
    \Действия: { Красный: 100%; Синий: 0%; }\n\
    \Убийство: { Красный: Красный; }\n\
    \Симпатии: { * < Красный: 100%; * < Синий: 100%; }\n\
    \Смертность: { Красный: 0%; Синий: 0%; *: 0%; }\n\
    \Долголетие: { Красный: 1000; Синий: 1000; *: 1000; }\n\
    \Успех убийства: { Красный: 100%; Синий: 100%; *: 100%; }\n\
    \Потомство: { Красный: 0: 100%; Синий: 0: 100%; *: 0: 100%; }\n"

report :: TestResult -> IO ()
report OK = putStrLn "  OK"
report (Fail n) = putStrLn ("  FAIL: " ++ n)
