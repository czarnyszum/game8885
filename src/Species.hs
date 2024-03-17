{-# LANGUAGE TemplateHaskell #-}

module Species where

import           Control.Lens
import           Control.Monad.State

import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Text           as T

import           System.Random

import           Probability.Sample

data Species b = Pure b | Mix b b deriving (Eq, Ord)

mix :: Ord b => b -> b -> Maybe (Species b)
mix x y | x < y = Just (Mix x y)
mix x y | x > y = Just (Mix y x)
mix _ _ = Nothing

genAll :: Ord b => [b] -> [Species b]
genAll bs =
       let
           p1 = map Pure bs
           p2 = catMaybes $ (pure mix) <*> bs <*> bs
       in
         p1 ++ p2

data Action = Kill | Fuck

data Space b = Space {
      _freeSpace  :: Int,
      _population :: M.Map (Species b) Int
    }
makeLenses ''Space

data Tables b = Tables {
      _randGen     :: StdGen,
      _spAll       :: [Species b],
      _killRatio   :: Generator Bool,
      _rapeRatio   :: Generator Bool,
      _space       :: Space b,
      _spColor     :: M.Map (Species b) T.Text,
      _actionGen   :: (Space b) -> Generator Action,
      _killGen     :: M.Map (Species b) ((Space b) -> Generator (Species b)),
      _fuckGen     :: M.Map (Species b) ((Space b) -> Generator (Species b)),
      _sympathyGen :: M.Map (Species b, Species b) (Generator Bool),
      _creationGen :: M.Map (Species b, Species b) (Generator (Species b))
    }
makeLenses ''Tables

type SpaceCtx b a = State (Tables b) a

newSpace :: Ord b => SpaceCtx b (Space b)
newSpace =
    do
      bs <- gets (view spAll)
      s <- gets (view space)
      let
          inj x = (x, 0)
          pop = M.fromList . map inj $ bs
          unoc = view freeSpace s
      return (Space unoc pop)

chooseAction :: Ord b => Species b -> Space b -> SpaceCtx b (Space b)
chooseAction x sp =
    do
      s <- gets (view space)
      actG <- gets (view actionGen)
      act <- sample randGen (actG s)
      case act of
        Kill -> killAction x sp
        Fuck -> fuckAction x sp

fuckAction :: Ord b => Species b -> Space b -> SpaceCtx b (Space b)
fuckAction x sp =
    do
      current <- gets (view space)
      fuckG <- gets (view fuckGen)
      symG <- gets (view sympathyGen)
      let
          fg = (fuckG M.! x) current
      y <- sample randGen fg
      let
          sym = symG M.! (x, y)
      outcome <- sample randGen sym
      if outcome
      then creationAction x y sp
      else rapeAction x y sp

creationAction :: Ord b => Species b -> Species b -> Space b -> SpaceCtx b (Space b)
creationAction x y sp =
    do
      cGen <- gets (view creationGen)
      z <- sample randGen (cGen M.! (x, y))
      let
          occ = over freeSpace pred
          currentUp = over population (M.adjust pred x . M.adjust pred y)
          newUp = over population (M.adjust succ x . M.adjust succ y . M.adjust succ z)
      modify (over space currentUp)
      return (occ . newUp $ sp)

rapeAction :: Ord b => Species b -> Species b -> Space b -> SpaceCtx b (Space b)
rapeAction x y sp = undefined


killAction :: Ord b => Species b -> Space b -> SpaceCtx b (Space b)
killAction x sp =
    do
      current <- gets (view space)
      kill <- gets (view killGen)
      ratio <- gets (view killRatio)
      let
          kG = (kill M.! x) current
      y <- sample randGen kG
      outcome <- sample randGen ratio
      let
          currentUp = if outcome then over population (M.adjust pred y) else over population (M.adjust pred x)
          occ = over freeSpace succ
          newUp = if outcome then over population (M.adjust succ x) else over population (M.adjust succ y)
      modify (over space currentUp)
      return (occ . newUp $ sp)
