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

-- | Field state.
--
--   * @_freeSpace@  — number of free slots on the field;
--   * @_population@ — alive chibiks per species;
--   * @_blocked@    — chibiks that cannot be chosen as partners this turn
--                     (rule 2: the two parents and the child of every
--                     reproduction are blocked until the end of the turn);
--   * @_cross@      — chibiks that just reproduced with a chibik of another
--                     color this turn (targets of the White kill behavior).
data Space b = Space {
      _freeSpace  :: Int,
      _population :: M.Map (Species b) Int,
      _blocked    :: M.Map (Species b) Int,
      _cross      :: M.Map (Species b) Int
    }
makeLenses ''Space

-- | Environment parameters (the "Параметры" section of a rule file).
data Env = Env {
      _envSpaceSize :: Int,       -- ^ Поле: total slots on the field
      _envInitial   :: Int,       -- ^ Начало: initial chibiks per base species
      _envWin       :: Int,       -- ^ Победа: species count that ends the game
      _envRape      :: Rational,  -- ^ Изнасилование: prob. of rape on refusal
      _envMaxSteps  :: Int        -- ^ Максимум шагов: step cap (0 = unlimited)
    } deriving (Eq, Show)
makeLenses ''Env

-- | Default environment: the original game parameters.
defaultEnv :: Env
defaultEnv = Env 888 5 555 (1 % 2) 0
