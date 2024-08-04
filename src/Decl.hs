module Decl where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

import           Data.List
import qualified Data.Map             as M
-- import           Data.Maybe
import qualified Data.Text            as T

import           Pattern
import           Species

type DeclColor = (T.Text, T.Text)

showColor :: DeclColor -> String
showColor (s, c) = (T.unpack s) ++ " ~ " ++ (T.unpack c)

type DeclBase = [T.Text]

showBase :: DeclBase -> String
showBase bs = concatMap (\t -> " " ++ T.unpack t) bs

type DeclSynonym = (T.Text, T.Text, T.Text)

showSyn :: DeclSynonym -> String
showSyn (x, y, z) = (T.unpack x) ++ " x " ++ (T.unpack y) ++ " ~ " ++ (T.unpack z)

type DeclCreation = (Pat T.Text, Pat T.Text, [(Pat T.Text, Rational)])

showCreation :: DeclCreation -> String
showCreation (p0, p1, cl) = (show p0) ++ " < "  ++ (show p1) ++ " -> " ++ (show cl)

type DeclSympathy = (Pat T.Text, [(Pat T.Text, Rational)])

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

baseAction :: DeclBase -> SpaceCtx T.Text ()
baseAction xs =
    let
        upB ss = sort (ss ++ xs)
        res t = M.insert t (Pure t)
        upR m = foldr res m xs
        up = over spBase upB .
             over nameResolver upR
    in
      modify up

synonymAction :: DeclSynonym -> SpaceCtx T.Text ()
synonymAction (t, t0, t1) =
    let
        upR = M.insert t (mix t0 t1)
        up = over nameResolver upR
    in
      modify up

mixAction :: SpaceCtx T.Text ()
mixAction =
    do
      ss <- gets (view spBase)
      let
          up = set spAll (genAll ss)
      modify up

resolveSpecies :: T.Text -> SpaceCtx T.Text (Species T.Text)
resolveSpecies s =
    do
      xs <- gets (view nameResolver)
      case M.lookup s xs of
        Nothing -> throwError (ErrorResolution s)
        Just sp -> return sp

colorAction :: DeclColor -> SpaceCtx T.Text ()
colorAction (s, c) =
    do
      sp <- resolveSpecies s
      let
          upC = M.insert sp c
          up = over spColor upC
      modify up



{-

matchSympathy :: DeclSympathy -> (Species T.Text, Species T.Text) -> Maybe Rational
matchSympathy (p0, ps) (s0, s1) =
    do
      subst0 <- patternMatch p0 s0
      let
          f (p, c) =
              do
                sub <- patternMatch p s1
                return (sub, c)
          substs = catMaybes $ map f ps
          msg0 = (show subst0) ++ "\nSecond part: " ++ (show (ps, s1))
          msg1 = show subst0
      case substs of
        []              -> trace ("\nSecond part failed. First match: " ++ msg0) Nothing
        (subst1, c) : _ ->
            if trace msg0 . trace msg1 $ all id $ M.intersectionWith (==) subst0 subst1
            then trace "Good" $ Just c
            else trace "Not good" $ Nothing

-}
