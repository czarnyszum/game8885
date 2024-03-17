{-# LANGUAGE MultiWayIf #-}

-- Copyright 2014 Romain Edelmann

module Probability.Distribution where


--import           Control.Arrow (second)
import qualified Data.Function as F
import           Data.List     (groupBy, sortBy)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)
--import           Data.Monoid
import           Data.Ord      (comparing)
import           Data.Set      (Set)

-- | Probability. Should be between 0 and 1.
type Probability = Rational

newtype Distribution a = Distribution
    { toMap :: Map a Probability } deriving Eq

instance Show a => Show (Distribution a) where
    show d = "fromList " ++ show (toList d)

--instance (Ord a, Monoid a) => Monoid (Distribution a) where
--    mempty = always mempty
--    mappend d1 d2 = combineWith mappend d1 d2

toList :: Distribution a -> [(a, Probability)]
toList (Distribution xs) = M.toAscList xs

size :: Distribution a -> Int
size = M.size . toMap

support :: Distribution a -> Set a
support = M.keysSet . toMap

fromList :: (Ord a, Real p) => [(a, p)] -> Distribution a
fromList xs = Distribution $ M.fromDistinctAscList $ zip vs scaledPs
  where
    as = map aggregate $ groupBy ((==) `F.on` fst) $ sortBy (comparing fst) xs
      where
        aggregate ys = let (v : _, qs) = unzip ys in
            (v, fromRational $ toRational $ sum qs)
    (vs, ps) = unzip $ filter ((> 0) . snd) as
    t = sum ps
    scaledPs = if t /= 1 then map (/ t) ps else ps

always :: a -> Distribution a
always x = Distribution $ M.singleton x 1

uniform :: (Ord a) => [a] -> Distribution a
uniform xs = fromList $ fmap (\ x -> (x, p)) xs
  where
    p = 1 / toRational (length xs)

withProbability :: Rational -> Distribution Bool
withProbability p = fromList [(False, (1 :: Rational) - p'), (True, p')]
  where
    p' = fromRational $ max 0 $ min 1 $ toRational p

dmap :: Ord b => (a -> b) -> Distribution a -> Distribution b
dmap f (Distribution xs) = Distribution $ M.mapKeysWith (+) f xs

assuming :: (a -> Bool) -> Distribution a -> Distribution a
assuming f (Distribution xs) = Distribution $ fmap adjust filtered
  where
    filtered = M.filterWithKey (const . f) xs
    adjust x = x * (1 / total)
    total = sum $ M.elems filtered

observing :: (a -> Distribution Bool) -> Distribution a -> Distribution a
observing f (Distribution xs) = Distribution $ fmap adjust filtered
  where
    filtered = M.filter (/= 0) $ M.mapWithKey tweak xs
    tweak x p = let
      Distribution px = f x
      pt = fromMaybe 0 $ M.lookup True px
      in pt * p
    adjust x = x * (1 / total)
    total = sum $ M.elems filtered

combineWith :: (Ord z) => (a -> b -> z) -> Distribution a -> Distribution b -> Distribution z
combineWith f (Distribution xs) (Distribution ys) = Distribution $ M.unionsWith (+) $ do
    (x, p) <- M.toList xs
    return $ M.fromListWith (+) $ do
        (y, q) <- M.toList ys
        return (f x y, p * q)

andThen :: Ord b => Distribution a -> (a -> Distribution b) -> Distribution b
andThen (Distribution xs) f = Distribution $
    M.unionsWith (+) $ fmap go $ M.toList xs
  where
    go (x, p) = fmap (* p) $ toMap $ f x
