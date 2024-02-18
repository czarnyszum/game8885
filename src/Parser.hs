
module Parser where

import           Data.Functor.Identity

import qualified Data.Text             as T
import qualified Data.Text.IO          as T

import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tk

import           Decl

style :: GenLanguageDef T.Text st Identity
style = Tk.LanguageDef
               { Tk.commentStart   = "/*"
               , Tk.commentEnd     = "*/"
               , Tk.commentLine    = "//"
               , Tk.nestedComments = True
               , Tk.identStart     = letter <|> char '_'
               , Tk.identLetter    = alphaNum <|> oneOf "_'"
               , Tk.opStart        = Tk.opLetter style
               , Tk.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
               , Tk.reservedOpNames= []
               , Tk.reservedNames  = []
               , Tk.caseSensitive  = True
               }

lexer :: Tk.GenTokenParser T.Text st Identity
lexer = Tk.makeTokenParser style

parens :: Parser a -> Parser a
parens  = Tk.parens lexer

braces :: Parser a -> Parser a
braces = Tk.braces lexer

brackets :: Parser a -> Parser a
brackets = Tk.brackets lexer

identifier :: Parser String
identifier = Tk.identifier lexer

whiteSpace :: Parser ()
whiteSpace = Tk.whiteSpace lexer

reserved :: String -> Parser ()
reserved = Tk.reserved lexer

symbol :: String -> Parser String
symbol = Tk.symbol lexer

parseBase :: Parser Decl
parseBase =
    do
      _ <- symbol "Базовые виды:"
      xs <- identifier `sepBy` (symbol ",")
      return . Base $ map T.pack xs

parseDecl :: Parser Decl
parseDecl =
    do
      _ <- whiteSpace
      parseBase


test :: IO ()
test =
    do
      let
          file = "rules/example.rule"
      content <- T.readFile file
      parseTest parseDecl content
