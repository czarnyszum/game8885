{-# LANGUAGE OverloadedStrings #-}
-- | Инструмент балансировки: статистика по набору правил.
--   Использование (сборка напрямую GHC из снапшотов, как build.sh):
--     GHC=~/.stack/programs/x86_64-linux/ghc-tinfo6-libc6-pre232-9.6.4/bin/ghc
--     PKGDB=~/.stack/snapshots/x86_64-linux-tinfo6-libc6-pre232/2ffcbd0a44e5bf2238ee3c1f5a61323b01b5e0f4d57520d2c7f8cf24fc62b261/9.6.4/pkgdb
--     "$GHC" -O1 -package-db "$PKGDB" -isrc tools/balance.hs -o .build/balance
--     .build/balance rules/8885A.rule 500
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
    args <- getArgs'
    let file = case args of { (f:_) -> f; [] -> "rules/8885.rule" }
        n = case args of { (_:s:_) -> read s; [] -> 100 }
    content <- TIO.readFile file
    let decls = either (error . show) id (parseRuleFile file content)
        t0 = either (error . show) id (compileDecls decls (mkStdGen 42))
        tbl0 = set space (initialSpace t0) t0
        speciesOrder = view spAll tbl0
        names = map (speciesName tbl0) speciesOrder
    seeds <- sequence (replicate n (randomRIO (1, 999999999) :: IO Int))
    results <- mapM (runOne tbl0) seeds
    -- outcome counts
    let wins = M.fromListWith (+) [ (outcomeLabel r, 1) | r <- results ]
    putStrLn ("=== " ++ file ++ " (" ++ show n ++ " партий) ===")
    mapM_ (\(k, v) -> putStrLn (k ++ ": " ++ show v)) (L.sortBy (\(a,_) (b,_) -> compare a b) (M.toList wins))
    let steps = map snd3 results
    putStrLn ("шагов: мин=" ++ show (minimum steps) ++ ", макс=" ++ show (maximum steps)
              ++ ", среднее=" ++ show (sum steps `div` length steps))
    -- average final population shares per species (per-game share, then averaged)
    let finals = map thd3 results
        avgShare s =
            let shares = [ if t > 0 then 100 * fromIntegral (M.findWithDefault 0 s (view (space.population) tbl)) / fromIntegral t else 0
                         | tbl <- finals, let t = sum (M.elems (view (space.population) tbl)) ]
            in sum shares / fromIntegral (length shares)
    putStrLn "средняя доля в финале (% от всех чибиков):"
    mapM_ (\(nm, sh) -> putStrLn ("  " ++ T.unpack nm ++ ": " ++ show (round sh :: Int) ++ "%"))
          (zip names (map avgShare speciesOrder))
  where
    getArgs' = getArgs

outcomeLabel :: (Maybe T.Text, Int, Tables T.Text) -> String
outcomeLabel (mres, _, _) = maybe "не завершена" T.unpack mres

snd3 (_, b, _) = b
thd3 (_, _, c) = c

runOne :: Tables T.Text -> Int -> IO (Maybe T.Text, Int, Tables T.Text)
runOne tbl0 seed = go 0 (set randGen (mkStdGen seed) tbl0)
  where
    go n tbl = do
        (mres, tbl') <- runStateT stepTurn tbl
        case mres of
          Just r  -> return (Just r, n + 1, tbl')
          Nothing -> if n >= 1000 then return (Nothing, n, tbl') else go (n + 1) tbl'
