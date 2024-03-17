{-# LANGUAGE TemplateHaskell #-}

module Species where

import           Control.Lens

import qualified Data.Map           as M
import           Data.Maybe
import qualified Data.Text          as T

import           Probability.Sample

data Species b = Pure b | Mix b b

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

data Tables b = Tables {
      _spColor   :: M.Map b T.Text,
      _actionGen :: Generator Action
    }
makeLenses ''Tables
