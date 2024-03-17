module Decl where

import qualified Data.Text as T

import           Pattern

type DeclColor = (T.Text, T.Text)

showColor :: DeclColor -> String
showColor (s, c) = (T.unpack s) ++ " ~ " ++ (T.unpack c)

type DeclBase = [T.Text]

showBase :: DeclBase -> String
showBase bs = concatMap (\t -> " " ++ T.unpack t) bs

type DeclSynonym = (T.Text, T.Text, T.Text)

showSyn :: DeclSynonym -> String
showSyn (x, y, z) = (T.unpack x) ++ " x " ++ (T.unpack y) ++ " ~ " ++ (T.unpack z)

type DeclCreation = (Pat T.Text, Pat T.Text, [(Pat T.Text, Int)])

showCreation :: DeclCreation -> String
showCreation (p0, p1, cl) = (show p0) ++ " < "  ++ (show p1) ++ " -> " ++ (show cl)

type DeclSympathy = (Pat T.Text, [(Pat T.Text, Int)])

data Decl =
    Base DeclBase |
    Synonym DeclSynonym |
    Creation DeclCreation |
    Sympathy DeclSympathy |
    Color DeclColor

showSympathy :: DeclSympathy -> String
showSympathy (p0, cl) = (show p0) ++ " < " ++ (show cl)

instance Show Decl where
    show (Base bs)     = "Базовые виды:" ++ (showBase bs)
    show (Synonym ss)  = "Синоним: " ++ showSyn ss
    show (Creation cs) = "Правило рождения: " ++ showCreation cs
    show (Sympathy ss) = "Правило симпатий: " ++ showSympathy ss
    show (Color cs)    = "Цвет: " ++ showColor cs


