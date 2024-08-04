
module Pattern where

import qualified Data.Map  as M
import qualified Data.Text as T

import           Species

data PatSp a = Any | Var a | ConstBase T.Text deriving Eq

instance Show a => Show (PatSp a) where
    show Any           = "(*)"
    show (Var v)       = show v
    show (ConstBase c) = T.unpack c

data Pat a = PAny | P1 (PatSp a) | P2 (PatSp a) (PatSp a) deriving Eq

instance Show a => Show (Pat a) where
    show PAny       = "*"
    show (P1 p)     = show p
    show (P2 p0 p1) = "(" ++ (show p0) ++ ", " ++ (show p1) ++ ")"

data ExpSp a = VarE a | ConstBaseE T.Text
data Exp a = E1 (ExpSp a) | E2 (ExpSp a) (ExpSp a)

-- any problem

patternMatch :: Ord a => Pat a -> Species T.Text -> Maybe (M.Map a T.Text)
patternMatch PAny _                = Just M.empty
patternMatch (P1 Any) (Pure _)     = Just M.empty
patternMatch (P1 (Var x)) (Pure s) = Just $ M.singleton x s
patternMatch (P1 (ConstBase t)) (Pure s) | t == s = Just M.empty
patternMatch (P2 Any Any) (Mix _ _) = Just M.empty
patternMatch (P2 Any (Var x)) (Mix _ s) = Just $ M.singleton x s
patternMatch (P2 Any (ConstBase t)) (Mix _ s) | t == s = Just M.empty
patternMatch (P2 (Var x) Any) (Mix s _) = Just $ M.singleton x s
patternMatch (P2 (Var x) (Var y)) (Mix t s) = Just $ M.fromList [(x, t), (y, s)]
patternMatch (P2 (Var x) (ConstBase t)) (Mix r s) | t == s = Just $ M.singleton x r
patternMatch (P2 (ConstBase r) Any) (Mix s _) | r == s = Just M.empty
patternMatch (P2 (ConstBase r) (Var x)) (Mix s t) | r == s = Just $ M.singleton x t
patternMatch (P2 (ConstBase u) (ConstBase v)) (Mix s t) | u == s || v == t = Just M.empty
patternMatch _ _ = Nothing

