
module Decl where

import qualified Data.Text as T

type DeclBase = [T.Text]

showBase :: DeclBase -> String
showBase bs = concatMap (\t -> " " ++ T.unpack t) bs

type DeclSynonym = (T.Text, T.Text, T.Text)

data Decl = Base DeclBase | Synonym DeclSynonym

showSyn :: DeclSynonym -> String
showSyn (x, y, z) = (T.unpack x) ++ " + " ++ (T.unpack y) ++ " -> " ++ (T.unpack z)

instance Show Decl where
    show (Base bs)    = "Базовые виды:" ++ (showBase bs)
    show (Synonym ss) = showSyn ss
