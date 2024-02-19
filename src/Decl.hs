
module Decl where

import qualified Data.Text as T

import           Pattern

type DeclBase = [T.Text]

showBase :: DeclBase -> String
showBase bs = concatMap (\t -> " " ++ T.unpack t) bs

type DeclSynonym = (T.Text, T.Text, T.Text)

showSyn :: DeclSynonym -> String
showSyn (x, y, z) = (T.unpack x) ++ " x " ++ (T.unpack y) ++ " -> " ++ (T.unpack z)

type DeclCreation = (Pat T.Text, Pat T.Text, [(Pat T.Text, Int)])

showCreation :: DeclCreation -> String
showCreation (p0, p1, cl) = (show p0) ++ " < "  ++ (show p1) ++ " -> " ++ (show cl)

data Decl = Base DeclBase | Synonym DeclSynonym | Creation DeclCreation

instance Show Decl where
    show (Base bs)     = "Базовые виды:" ++ (showBase bs)
    show (Synonym ss)  = showSyn ss
    show (Creation cs) = showCreation cs
