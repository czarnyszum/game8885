
module Pattern where

import qualified Data.Text as T

data PatSp a = Any | Var a | ConstBase T.Text

instance Show a => Show (PatSp a) where
    show Any           = "*"
    show (Var v)       = show v
    show (ConstBase c) = T.unpack c

data Pat a = P1 (PatSp a) | P2 (PatSp a) (PatSp a)

instance Show a => Show (Pat a) where
    show (P1 p)     = show p
    show (P2 p0 p1) = "(" ++ (show p0) ++ ", " ++ (show p1) ++ ")"

