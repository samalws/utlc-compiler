-- no, I'm not commenting this one either

module Compiler.Parser where

import Compiler.CodeConversion
import Text.Parsec
import Text.Parsec.String
import Data.Maybe

allowedSpecialChars = "~!@#$%^&*_+|:,<./?"
-- TO ADD: =, \, -, >, maybe ;

commentParser :: Parser ()
commentParser = char '[' >> many (noneOf "]") >> char ']' >> pure ()

whitespace1 :: Parser ()
whitespace1 = many1 (commentParser <|> (space >> pure ())) >> pure ()

whitespace :: Parser ()
whitespace = many (commentParser <|> (space >> pure ())) >> pure ()

varParser :: Parser Var
varParser = many1 $ alphaNum <|> oneOf allowedSpecialChars

exprParser :: Parser Expr0
exprParser = try appsParser <|> try lamParser

lamParser = do
  arg <- many1 letter
  whitespace1
  string "->"
  whitespace1
  expr <- exprParser
  pure $ Lam0 arg expr

appsParser = do
  exprs <- sepBy subAppsParser (whitespace1)
  let backticks    = filter        isBacktickVar  exprs
  let nonBackticks = filter (not . isBacktickVar) exprs
  pure $ convList exprs Nothing
  where
    subAppsParser = try parenExprParser <|> try varExprParser
    isBacktickVar (Var0 ('`':s)) = True
    isBacktickVar _ = False
    unBacktickVar (Var0 ('`':s)) = Var0 s
    unBacktickVar a = a
    convList (h:[]) Nothing  = unBacktickVar h
    convList (h:[]) (Just e) = e `App0` unBacktickVar h
    convList (h:r) Nothing = convList r (Just $ unBacktickVar h)
    convList (h:r) (Just e)
      | isBacktickVar h = (unBacktickVar h `App0` e) `App0` convList r Nothing
      | otherwise = convList r (Just $ e `App0` h)
parenExprParser = do
  char '('
  expr <- exprParser
  char ')'
  pure expr
varExprParser = do
  backtick <- maybe "" pure <$> optionMaybe (char '`')
  var <- (backtick <>) <$> varParser
  pure $ Var0 var

lineParser :: Parser Line0
lineParser = do
  name <- varParser
  whitespace1
  char '='
  whitespace1
  exp <- exprParser
  whitespace
  char ';'
  whitespace
  pure $ Line0 name exp

codeParser :: Parser Code0
codeParser = Code0 <$> many1 lineParser

codeFileParser :: Parser Code0
codeFileParser = do
  whitespace
  code <- codeParser
  whitespace
  eof
  pure code
