
module Patter where

import qualified Data.Text as T

data PatSp a = Any | Var a | ConstBase T.Text | ConstMix T.Text T.Text

data Pat a = P1 (PatSp a) | P2 (PatSp a) (PatSp a)
