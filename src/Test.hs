{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the game simulation: DSL parsing/compilation, probability
--   distributions (rule 4), rape mechanic, and end-of-game invariants.
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

spCount :: Species T.Text -> Tables T.Text -> Int
spCount s tbl = M.findWithDefault 0 s (view (space.population) tbl)

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
        total = sum (M.elems pop)
        envSize = view (env.envSpaceSize) tbl
        allN = all (>= 0) (M.elems pop)
        blkOk = and [ M.findWithDefault 0 s blk <= M.findWithDefault 0 s pop | s <- M.keys blk ]
        crsOk = and [ M.findWithDefault 0 s crs <= M.findWithDefault 0 s pop | s <- M.keys crs ]
    in free + total == envSize && allN && blkOk && crsOk

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
            sRed = Pure "Красный"
            sBlue = Pure "Синий"
            sPurple = Mix "Красный" "Синий"
        putStrLn ("  базовых видов: " ++ show (length bases))
        putStrLn ("  всего видов (включая гибриды): " ++ show (length allSp))
        putStrLn ("  Фиолетовый разрешён: " ++ show (M.lookup "Фиолетовый" resolver == Just sPurple))

        -- initial field
        let tbl1 = set space (initialSpace tbl0) tbl0
            sp0 = view space tbl1
        putStrLn ("  начальное поле: свободно=" ++ show (view freeSpace sp0)
                  ++ ", красных=" ++ show (spCount sRed tbl1)
                  ++ ", всего=" ++ show (sum (M.elems (view population sp0))))
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
        putStrLn "== Полные партии =="
        seeds <- sequence (replicate 5 (randomRIO (1, 1000000) :: IO Int))
        results <- mapM runOneGame seeds
        mapM_ (\(seed, (mres, nsteps, _)) ->
                  putStrLn ("  seed=" ++ show seed ++ ": шагов=" ++ show nsteps
                            ++ ", результат=" ++ maybe "не завершена" T.unpack mres))
               (zip seeds results)
        let finished = all (\(r, _, _) -> maybe False (const True) r) results
        check "все партии завершились" finished >>= report

        putStrLn "== Готово =="

sWhite :: Species T.Text
sWhite = Pure "Белый"

sYellow :: Species T.Text
sYellow = Pure "Жёлтый"

sBlue :: Species T.Text
sBlue = Pure "Синий"

sBlack :: Species T.Text
sBlack = Pure "Чёрный"

runOneGame :: Int -> IO (Maybe T.Text, Int, Tables T.Text)
runOneGame seed = do
    rt <- loadTables "rules/8885.rule"
    case rt of
      Left err -> error err
      Right tbl0 -> do
          let tbl1 = set space (initialSpace tbl0) tbl0
              tbl2 = set randGen (mkStdGen seed) tbl1
          runToEnd tbl2 100000

report :: TestResult -> IO ()
report OK = putStrLn "  OK"
report (Fail n) = putStrLn ("  FAIL: " ++ n)
