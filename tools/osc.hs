{-# LANGUAGE OverloadedStrings #-}
-- | Инструмент для подбора параметров: измеряет динамику популяции
--   (осцилляции) и средний возраст смерти.
--   Использование (сборка напрямую GHC, как build.sh):
--     GHC=.../ghc-9.6.4; PKGDB=.../9.6.4/pkgdb
--     "$GHC" -O1 -package-db "$PKGDB" -isrc tools/osc.hs -o .build/osc
--     .build/osc rules/triplet.rule 50 400
--   Печатает: распределение исходов, длину партий, число циклов роста/спада
--   (меру осцилляции), средний возраст смерти.
module Main where
import Control.Lens
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import System.Random (mkStdGen, randomRIO)
import Decl
import Parser
import Pattern
import Sim
import Species
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let file = case args of { (f:_) -> f; [] -> "rules/triplet.rule" }
        n    = case args of { (_:s:_) -> read s; [] -> 30 }
        maxS = case args of { (_:_:m:_) -> read m; [] -> 300 }
    content <- TIO.readFile file
    let decls = either (error . show) id (parseRuleFile file content)
        t0 = either (error . show) id (compileDecls decls (mkStdGen 42))
        tbl0 = set space (initialSpace t0) t0
    seeds <- sequence (replicate n (randomRIO (1, 999999999) :: IO Int))
    results <- mapM (runOne tbl0 maxS) seeds
    let wins = M.fromListWith (+) [ (outcomeLabel r, 1) | r <- results ]
    putStrLn ("=== " ++ file ++ " (" ++ show n ++ " партий, до " ++ show maxS ++ " ходов) ===")
    mapM_ (\(k, v) -> putStrLn ("  исход: " ++ k ++ ": " ++ show v))
          (L.sortBy (\(a,_) (b,_) -> compare a b) (M.toList wins))
    let lens = map snd3 results
        cycles = map trd3 results
    putStrLn ("  длина партии: мин=" ++ show (minimum lens) ++ ", медиана="
              ++ show (median lens) ++ ", макс=" ++ show (maximum lens))
    putStrLn ("  циклы роста/спада (мера осцилляции): медиана=" ++ show (median cycles)
              ++ ", в среднем=" ++ show (round (fromIntegral (sum cycles) / fromIntegral (length cycles) :: Double)))
    putStrLn ("  партий с >=2 циклами: " ++ show (length (filter (>= 2) cycles))
              ++ " из " ++ show (length cycles))
    -- средний возраст смерти (по всем партиям)
    let allHists = concatMap (M.elems . view (space.histogram) . fst4) results
        ages = [ (a, k) | m <- allHists, (a, k) <- M.toList m ]
        total = sum (map snd ages)
        meanAge = if total == 0 then 0 else fromIntegral (sum (map (\(a,k) -> a*k) ages)) / fromIntegral total
    putStrLn ("  средний возраст смерти: " ++ show (round (meanAge :: Double)))
  where
    fst4 (_, _, _, c) = c

median :: [Int] -> Int
median xs = let ys = L.sort xs; n = length ys in ys !! (n `div` 2)

outcomeLabel :: (Maybe T.Text, Int, Int, Tables T.Text) -> String
outcomeLabel (mres, _, _, _) = maybe "не завершена" T.unpack mres

data Watch = WatchPeak | WatchTrough

-- | Number of boom-bust cycles in a population series: a local maximum whose
--   following value drops by at least 30% counts as a cycle once the series
--   then recovers to at least 130% of the trough.
countCycles :: [Int] -> Int
countCycles series = go WatchPeak 0 0 series
  where
    go _ n _ [] = n
    go WatchPeak n mx (x : rest)
        | x < mx * 70 `div` 100 = go WatchTrough (n + 1) x rest
        | otherwise             = go WatchPeak n (max mx x) rest
    go WatchTrough n mn (x : rest)
        | x > mn * 130 `div` 100 = go WatchPeak n x rest
        | otherwise              = go WatchTrough n (min mn x) rest

runOne :: Tables T.Text -> Int -> Int -> IO (Maybe T.Text, Int, Int, Tables T.Text)
runOne tbl0 maxS seed = go 0 (set randGen (mkStdGen seed) tbl0) []
  where
    go n tbl series
        | n >= maxS = return (Nothing, n, 0, tbl)
        | otherwise = do
            (mres, tbl') <- runStateT stepTurn tbl
            let tot = sum (map length (M.elems (view (space.population) tbl')))
                series' = tot : series
            case mres of
              Just r  -> return (Just r, n + 1, countCycles (reverse series'), tbl')
              Nothing -> go (n + 1) tbl' series'

snd3 (_, b, _, _) = b
trd3 (_, _, c, _) = c
