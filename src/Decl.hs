
module Decl where

import qualified Data.Text as T

import           Pattern

type DeclBase = [T.Text]

showBase :: DeclBase -> String
showBase bs = concatMap (\t -> " " ++ T.unpack t) bs

type DeclSynonym = (T.Text, T.Text, T.Text)

type DeclCreation = (Pat T.Text, Pat T.Text, [(Pat T.Text, Double)])

data Decl = Base DeclBase | Synonym DeclSynonym | Creation DeclCreation

showSyn :: DeclSynonym -> String
showSyn (x, y, z) = (T.unpack x) ++ " x " ++ (T.unpack y) ++ " -> " ++ (T.unpack z)

instance Show Decl where
    show (Base bs)    = "Базовые виды:" ++ (showBase bs)
    show (Synonym ss) = showSyn ss
