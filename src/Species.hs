{-# LANGUAGE TemplateHaskell #-}

-- | Core domain types for the game 8885: species, field state and
--   environment parameters. This module is intentionally free of rule
--   interpretation; rules live in "Pattern" and the simulation in "Sim".
module Species where

import           Control.Lens

import qualified Data.List   as L
import qualified Data.Map    as M
import           Data.Ratio  ((%))

-- | A species: a pure (base) species or a hybrid of two base species.
data Species b = Pure b | Mix b b deriving (Eq, Ord, Show)

-- | Normalized hybrid constructor (components kept sorted).
mix :: Ord b => b -> b -> Species b
mix x y | x < y = Mix x y
mix x y | x > y = Mix y x
mix _ _ = error "Species.mix: mixing identical species"

isPure :: Species b -> Bool
isPure (Pure _) = True
isPure _        = False

isHybrid :: Species b -> Bool
isHybrid = not . isPure

-- | All unordered pairs of a list (the components are distinct).
genPairs :: [b] -> [(b, b)]
genPairs []       = []
genPairs (u : us) = map (\y -> (u, y)) us ++ genPairs us

-- | All species: every base species plus every hybrid of two distinct bases.
genAll :: Ord b => [b] -> [Species b]
genAll bs =
    let ss = L.sort bs
    in map Pure ss ++ map (uncurry mix) (genPairs ss)

-- | The two possible actions a chibik may take.
data Action = Kill | Fuck deriving (Eq, Show)

-- | The life state of an individual chibik:
--
--   * @_lifeAge@   — number of turns it has existed (age);
--   * @_lifeDeath@ — its current probability of dying at the next death
--                     check (the end of the turn). Grows every turn per the
--                     aging law, drops on a successful kill (reward) and
--                     grows on a successful reproduction (penalty).
data Life = Life {
      _lifeAge   :: Int,
      _lifeDeath :: Rational
    } deriving (Eq, Show)
makeLenses ''Life

-- | Field state.
--
--   * @_freeSpace@  — number of free slots on the field;
--   * @_population@ — alive chibiks per species, tracked individually
--                     (each chibik carries its age and death probability);
--   * @_blocked@    — chibiks that cannot be chosen as partners this turn
--                     (rule 2: the two parents and the child of every
--                     reproduction are blocked until the end of the turn);
--   * @_cross@      — chibiks that just reproduced with a chibik of another
--                     color this turn (targets of the White kill behavior);
--   * @_histogram@  — ages at death per species (age -> number of chibiks
--                     of that species that died at that age), accumulated
--                     over the whole game for the lifespan histograms.
data Space b = Space {
      _freeSpace  :: Int,
      _population :: M.Map (Species b) [Life],
      _blocked    :: M.Map (Species b) Int,
      _cross      :: M.Map (Species b) Int,
      _histogram  :: M.Map (Species b) (M.Map Int Int)
    }
makeLenses ''Space

-- | Environment parameters (the "Параметры" section of a rule file).
data Env = Env {
      _envSpaceSize    :: Int,       -- ^ Поле: total slots on the field
      _envInitial      :: Int,       -- ^ Начало: initial chibiks per base species
      _envWin          :: Int,       -- ^ Победа: species count that ends the game
      _envRape         :: Rational,  -- ^ Изнасилование: prob. of rape on refusal
      _envMaxSteps     :: Int,       -- ^ Максимум шагов: step cap (0 = unlimited)
      _envKillReward   :: Rational,  -- ^ Награда за убийство: death-prob. drop
                                     --   of the killer after a successful kill
      _envReproPenalty :: Rational   -- ^ Штраф за размножение: death-prob. rise
                                     --   of the actor after a successful
                                     --   reproduction (non-zero offspring)
    } deriving (Eq, Show)
makeLenses ''Env

-- | Default environment: the original game parameters plus the new
--   lifespan mechanics (kill reward 5%, reproduction penalty 10%).
defaultEnv :: Env
defaultEnv = Env 888 5 555 (1 % 2) 0 (1 % 20) (1 % 10)
