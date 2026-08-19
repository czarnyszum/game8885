{-# LANGUAGE OverloadedStrings #-}

-- | Parser for the rule DSL. Sections (each optional):
--
--   @Базовые виды@, @Синонимы@, @Цвета@, @Параметры@, @Действия@
--   (alias: @Склонность к убийству@), @Партнёры@, @Убийство@,
--   @Симпатии@, @Рождение@.
module Parser where

import           Control.Monad

import           Data.Char   (isUpper)
import           Data.Functor.Identity

import qualified Data.Text             as T

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
           , Tk.identLetter    = alphaNum <|> oneOf "_'-"
           , Tk.opStart        = Tk.opLetter style
           , Tk.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , Tk.reservedOpNames= []
           , Tk.reservedNames  = ["кроме"]
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

-- ---------------------------------------------------------------------------
-- Tokens and patterns

parseVarOrConst :: Parser (PatSp T.Text)
parseVarOrConst =
    do
      xs <- identifier
      if all isUpper xs
      then return . Var . T.pack $ xs
      else return . ConstName . T.pack $ xs

parseTildeToken :: Parser (PatSp T.Text)
parseTildeToken =
    do
      _ <- symbol "~"
      n <- identifier
      _ <- symbol "~"
      case n of
        "я"            -> return SelfPat
        "родители"     -> return ParentsPat
        "чистый"       -> return PurePat
        "гибрид"       -> return HybridPat
        "доминирующий" -> return DominantPat
        "смешанные"    -> return CrossPat
        _ -> fail ("Неизвестный специальный паттерн: ~" ++ n ++ "~")

parseToken :: Parser (PatSp T.Text)
parseToken = try parseTildeToken <|> parseVarOrConst <|> (fmap (const Any) $ symbol "*")

parseP0 :: Parser (Pat T.Text)
parseP0 = fmap (const PAny) $ symbol "*"

parseP1 :: Parser (Pat T.Text)
parseP1 = fmap P1 parseToken

parseP2 :: Parser (Pat T.Text)
parseP2 =
    do
      parens $
             do
               p0 <- parseToken
               _ <- symbol ","
               p1 <- parseToken
               return (P2 p0 p1)

parsePattern :: Parser (Pat T.Text)
parsePattern = try parseP0 <|> try parseP1 <|> parseP2

-- ---------------------------------------------------------------------------
-- Conditions and probability clauses

parseCond :: Parser Cond
parseCond =
    brackets $
           do
             n <- identifier
             op <- choice [try (symbol "<="), try (symbol ">="), symbol "<", symbol ">"]
             k <- integer
             case op of
               "<"  -> return (CondLess (T.pack n) k)
               "<=" -> return (CondLeq (T.pack n) k)
               ">"  -> return (CondGreater (T.pack n) k)
               ">=" -> return (CondGeq (T.pack n) k)
               _    -> fail "Некорректное условие"

parseProbCond :: Parser (Pat T.Text, Int, Maybe Cond)
parseProbCond =
    do
      p <- parsePattern
      _ <- symbol ":"
      x <- integer
      _ <- symbol "%"
      mcond <- option Nothing (Just <$> parseCond)
      return (p, x, mcond)

parseProb :: Parser (Pat T.Text, Int)
parseProb =
    do
      p <- parsePattern
      _ <- symbol ":"
      x <- integer
      _ <- symbol "%"
      return (p, x)

-- ---------------------------------------------------------------------------
-- Sections

parseBase :: Parser [Decl]
parseBase =
    do
      _ <- symbol "Базовые виды:"
      xs <- identifier `sepBy` (symbol ",")
      _ <- symbol ";"
      return [DeclBase (map T.pack xs)]

parseSynonymLine :: Parser (T.Text, T.Text, T.Text)
parseSynonymLine =
    do
      x <- identifier
      _ <- symbol "x"
      y <- identifier
      _ <- symbol "~"
      z <- identifier
      _ <- symbol ";"
      return (T.pack x, T.pack y, T.pack z)

parseSynonyms :: Parser [Decl]
parseSynonyms =
    do
      _ <- symbol "Синонимы:"
      braces $
             do
               _ <- whiteSpace
               ss <- parseSynonymLine `sepBy` whiteSpace
               return [DeclSynonym s | s <- ss]

parseColorLine :: Parser (T.Text, T.Text)
parseColorLine =
    do
      xs <- identifier
      _ <- symbol "~"
      _ <- symbol "#"
      cs <- manyTill (oneOf "0123456789abcdefABCDEF") (symbol ";")
      return (T.pack xs, T.pack ('#' : cs))

parseColors :: Parser [Decl]
parseColors =
    do
      _ <- symbol "Цвета:"
      braces $
             do
               _ <- whiteSpace
               ss <- parseColorLine `sepBy` whiteSpace
               return [DeclColor c | c <- ss]

-- | A parameter value: an integer or a percentage.
parseParamValue :: Parser T.Text
parseParamValue =
    do
      x <- integer
      pct <- option "" (symbol "%")
      return (T.pack (show x ++ pct))

-- | A parameter name may consist of several words ("Максимум шагов").
--   Parsed with raw letters because the lexer's identifier consumes
--   trailing whitespace.
parseParamName :: Parser T.Text
parseParamName = do
    cs <- many1 (letter <|> char ' ')
    return (T.strip (T.pack cs))

parseParamLine :: Parser (T.Text, T.Text)
parseParamLine =
    do
      name <- parseParamName
      _ <- symbol ":"
      v <- parseParamValue
      _ <- symbol ";"
      return (name, v)

parseParams :: Parser [Decl]
parseParams =
    do
      _ <- symbol "Параметры:"
      braces $
             do
               _ <- whiteSpace
               ps <- parseParamLine `sepBy` whiteSpace
               return [DeclParams ps]

-- | A rule name: a species name or the default "*" entry.
parseRuleName :: Parser T.Text
parseRuleName = (try (symbol "*" >> return "*")) <|> (T.pack <$> identifier)

-- | Action line: "Имя: 30%;", "Имя: ~смешанные~;" or "Имя: 60% ~смешанные~;".
parseActionLine :: Parser (T.Text, ActionSpec)
parseActionLine =
    do
      n <- parseRuleName
      _ <- symbol ":"
      spec <-
          (try $ do
             k <- integer
             _ <- symbol "%"
             m <- option Nothing $ Just <$> parseCrossToken
             case m of
               Nothing  -> return (ActionPercent k)
               Just ()  -> return (ActionCrossPercent k))
          <|> (do
                 _ <- parseCrossToken
                 return ActionCross)
      _ <- symbol ";"
      return (n, spec)
  where
    parseCrossToken = do
        _ <- symbol "~"
        w <- identifier
        _ <- symbol "~"
        if w == "смешанные" then return ()
        else fail "Ожидается ~смешанные~"

parseActions :: Parser [Decl]
parseActions =
    do
      _ <- (symbol "Действия:" <|> symbol "Склонность к убийству:")
      braces $
             do
               _ <- whiteSpace
               as <- parseActionLine `sepBy` whiteSpace
               return [DeclActions as]

parseTokenSet :: Parser [PatSp T.Text]
parseTokenSet = brackets (parseToken `sepBy` (symbol ","))

parsePartnerLine :: Parser (T.Text, PartnerSpec)
parsePartnerLine =
    do
      n <- parseRuleName
      _ <- symbol ":"
      pref <- parseTokenSet
      _ <- symbol "->"
      fb <- parseTokenSet
      mcond <- option Nothing (Just <$> parseCond)
      _ <- symbol ";"
      return (n, PartnerSpec pref fb mcond)

parsePartners :: Parser [Decl]
parsePartners =
    do
      _ <- symbol "Партнёры:"
      braces $
             do
               _ <- whiteSpace
               ps <- parsePartnerLine `sepBy` whiteSpace
               return [DeclPartners ps]

parseKillLine :: Parser (T.Text, KillSpec)
parseKillLine =
    do
      n <- parseRuleName
      _ <- symbol ":"
      ts <- parseToken `sepBy` (symbol ",")
      ex <- option [] (reserved "кроме" >> (identifier `sepBy` (symbol ",")))
      _ <- symbol ";"
      return (n, KillSpec ts (map T.pack ex))

parseKills :: Parser [Decl]
parseKills =
    do
      _ <- symbol "Убийство:"
      braces $
             do
               _ <- whiteSpace
               ks <- parseKillLine `sepBy` whiteSpace
               return [DeclKills ks]

parseSympathyLine :: Parser SympathySpec
parseSympathyLine =
    do
      p0 <- parsePattern
      _ <- symbol "<"
      clause <- manyTill parseProbCond (symbol ";")
      return (SympathySpec p0 clause)

parseSympathies :: Parser [Decl]
parseSympathies =
    do
      _ <- symbol "Симпатии:"
      braces $
             do
               _ <- whiteSpace
               ps <- parseSympathyLine `sepBy` whiteSpace
               return [DeclSympathies ps]

parseCreationLine :: Parser CreationSpec
parseCreationLine =
    do
      p0 <- parsePattern
      _ <- symbol "<"
      p1 <- parsePattern
      _ <- symbol "->"
      clause <- manyTill parseProb (symbol ";")
      return (CreationSpec p0 p1 clause)

parseCreations :: Parser [Decl]
parseCreations =
    do
      _ <- symbol "Рождение:"
      braces $
             do
               _ <- whiteSpace
               ps <- parseCreationLine `sepBy` whiteSpace
               return [DeclCreations ps]

parseDecl :: Parser [Decl]
parseDecl =
    do
      _ <- whiteSpace
      try parseBase <|> try parseParams <|> try parseSynonyms <|> try parseColors
          <|> try parseActions <|> try parsePartners <|> try parseKills
          <|> try parseCreations <|> parseSympathies

parseDecls :: Parser [Decl]
parseDecls = fmap join $ many1 parseDecl

-- | Parse a rule file (also allows trailing whitespace/comments).
parseRuleFile :: FilePath -> T.Text -> Either String [Decl]
parseRuleFile file content =
    case parse (whiteSpace >> parseDecls <* eof) file content of
      Left err  -> Left (show err)
      Right ds  -> Right ds
