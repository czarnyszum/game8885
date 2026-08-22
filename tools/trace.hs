{-# LANGUAGE OverloadedStrings #-}
-- | Трассировка: печатает численность видов через каждые k ходов
--   для нескольких партий. Используется для проверки появления гибридов.
--   Сборка как build.sh; запуск: .build/trace rules/triplet.rule 3 300 25
module Main where
import Control.Lens
import Control.Monad.State
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import System.Random (mkStdGen)
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
        n    = case args of { (_:s:_) -> read s; [] -> 3 }
        maxS = case args of { (_:_:m:_) -> read m; [] -> 300 }
        k    = case args of { (_:_:_:kv:_) -> read kv; [] -> 25 }
    content <- TIO.readFile file
    let decls = either (error . show) id (parseRuleFile file content)
        t0 = either (error . show) id (compileDecls decls (mkStdGen 42))
        tbl0 = set space (initialSpace t0) t0
        names = map (speciesName tbl0) (view spAll tbl0)
    mapM_ (\seed -> do
              putStrLn ("=== seed " ++ show seed ++ " ===")
              putStrLn ("шаг\t" ++ L.intercalate "\t" (map T.unpack names))
              (mres, _) <- go maxS k 0 (set randGen (mkStdGen seed) tbl0)
              putStrLn ("итог: " ++ maybe "не завершена" T.unpack mres))
          [1000 .. 1000 + n - 1]
  where
    go maxS k step tbl
        | step >= maxS = do
            printRow step tbl
            return (Nothing, tbl)
        | otherwise = do
            (mres, tbl') <- runStateT stepTurn tbl
            if step `mod` k == 0 then printRow step tbl' else return ()
            case mres of
              Just r  -> return (Just r, tbl')
              Nothing -> go maxS k (step + 1) tbl'
    printRow step tbl = do
        let sp = view space tbl
            counts = [ show (length (M.findWithDefault [] s (view population sp)))
                     | s <- view spAll tbl ]
        putStrLn (show step ++ "\t" ++ L.intercalate "\t" counts)
