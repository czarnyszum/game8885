
module Decl where

import qualified Data.Text as T

type DeclBase = [T.Text]

type DeclSynonym = (T.Text, T.Text, String)

data Decl = Base DeclBase | Synonym DeclSynonym

instance Show Decl where
    show (Base bs)    = show bs
    show (Synonym ss) = show ss
