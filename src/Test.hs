
module Test where

import           Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import           Text.Parsec

import           Decl
import           Parser
import           Species

test :: IO ()
test =
    do
      let
          file = "rules/example.rule"
      content <- T.readFile file
      case parse parseDecls file content of
        Left err -> putStrLn (show err)
        Right ds ->
            do
--              print ds
              let
                  s0 = Pure (T.pack "Желтый")
                  s1 = fromJust $ mix (T.pack "Желтый") (T.pack "Красный")
                  chk (Sympathy ss) = (show ss) ++ " againts " ++ (show $ matchSympathy ss (s0, s1))
                  chk _             = ""
                  flt (Sympathy _) = True
                  flt _            = False
              mapM_ (putStrLn . chk) (filter flt ds)

