{-# LANGUAGE OverloadedStrings #-}
-- | Валидация закона старения: гоняет только фазу старения/смерти (без
--   действий) и сообщает средний возраст смерти свободного чибика —
--   сравнение с теорией из SIMS.md.
--   Использование:
--     "$GHC" -O1 -package-db "$PKGDB" -isrc tools/lifespan.hs -o .build/lifespan
--     .build/lifespan rules/triplet.rule 400 40
module Main where
import Control.Lens
import Control.Monad
import Control.Monad.State
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
        rounds = case args of { (_:s:_) -> read s; [] -> 400 }
        reps = case args of { (_:_:r:_) -> read r; [] -> 40 }
    content <- TIO.readFile file
    let decls = either (error . show) id (parseRuleFile file content)
        t0 = either (error . show) id (compileDecls decls (mkStdGen 42))
        tbl0 = set space (initialSpace t0) t0
        allSp = view spAll tbl0
    merged <- foldM (runAll tbl0 rounds) M.empty [1 .. reps]
    putStrLn ("=== Старение без действий: " ++ file
              ++ ", " ++ show rounds ++ " ходов x " ++ show reps ++ " прогонов ===")
    mapM_ (report tbl0 merged) allSp
  where
    runAll tbl0' rounds acc k = do
        (_, tbl') <- runStateT (replicateM_ rounds ageAndDie)
                               (set randGen (mkStdGen (1000 + k)) tbl0')
        return (M.unionWith (M.unionWith (+)) acc (view (space.histogram) tbl'))
    report tbl0' m s =
        let h = M.findWithDefault M.empty s m
            total = sum (M.elems h)
            meanAge = if total == 0 then 0
                      else fromIntegral (sum (map (\(a, k) -> a * k) (M.toList h)))
                           / fromIntegral total
            -- выживаемость: доля, дожившая до возраста t
            surv t = if total == 0 then 0
                     else 100 * fromIntegral (sum [ k | (a, k) <- M.toList h, a >= t ])
                          / fromIntegral total
        in putStrLn ("  " ++ T.unpack (speciesName tbl0' s)
                     ++ ": умерло=" ++ show total
                     ++ ", средний возраст=" ++ show (round (meanAge :: Double))
                     ++ ", выживаемость: t=5 -> " ++ show (round (surv 5 :: Double))
                     ++ "%, t=10 -> " ++ show (round (surv 10 :: Double)) ++ "%")
