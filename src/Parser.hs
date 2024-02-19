
module Parser where

import           Control.Monad

import           Data.Functor.Identity

import           Data.Char
import qualified Data.Text             as T
import qualified Data.Text.IO          as T


import           Text.Parsec
import           Text.Parsec.Language
import           Text.Parsec.Text
import qualified Text.Parsec.Token     as Tk

import           Decl
import           Pattern

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

integer :: Parser Int
integer = fmap fromIntegral $ Tk.natural lexer


parseVarOrConst :: Parser (PatSp T.Text)
parseVarOrConst =
    do
      xs <- identifier
      if all isUpper xs
      then return . Var . T.pack $ xs
      else return . ConstBase . T.pack $ xs

parseSp :: Parser (PatSp T.Text)
parseSp = parseVarOrConst <|> (fmap (const Any) $ symbol "*")

parseP2 :: Parser (Pat T.Text)
parseP2 =
    do
      parens $
             do
               p0 <- parseSp
               _ <- symbol ","
               p1 <- parseSp
               return (P2 p0 p1)


parsePattern :: Parser (Pat T.Text)
parsePattern = (fmap P1 parseSp) <|> parseP2

parseProb :: Parser ((Pat T.Text), Int)
parseProb =
    do
      p <- parsePattern
      _ <- symbol ":"
      x <- integer
      _ <- symbol "%"
      return (p, x)

-- DeclCreation
parseCreationLine :: Parser DeclCreation
parseCreationLine =
    do
      p0 <- parsePattern
      _ <- symbol "<"
      p1 <- parsePattern
      _ <- symbol "->"
      clause <- manyTill parseProb (symbol ";")
      return (p0, p1, clause)


parseCreations :: Parser [Decl]
parseCreations =
    do
      _ <- symbol "Рождение:"
      braces $
             do
               _ <- whiteSpace
               ps <- parseCreationLine `sepBy` whiteSpace
               return (map Creation ps)

parseBase :: Parser [Decl]
parseBase =
    do
      _ <- symbol "Базовые виды:"
      xs <- identifier `sepBy` (symbol ",")
      return [ Base . map T.pack $ xs ]

parseSynonymLine :: Parser DeclSynonym
parseSynonymLine =
    do
      x <- identifier
      _ <- symbol "x"
      y <- identifier
      _ <- symbol "->"
      z <- identifier
      return (T.pack x, T.pack y, T.pack z)

parseSynonyms :: Parser [Decl]
parseSynonyms =
    do
      _ <- symbol "Синонимы:"
      braces $
             do
               _<- whiteSpace
               ss <- parseSynonymLine `sepBy` whiteSpace
               return (map Synonym ss)

parseDecl :: Parser [Decl]
parseDecl =
    do
      _ <- whiteSpace
      parseBase <|> parseSynonyms <|> parseCreations

parseDecls :: Parser [Decl]
parseDecls = fmap join $ many1 parseDecl

test :: IO ()
test =
    do
      let
          file = "rules/example.rule"
      content <- T.readFile file
      case parse parseDecls file content of
        Left err -> putStrLn (show err)
        Right ds -> mapM_ (putStrLn . show) ds
