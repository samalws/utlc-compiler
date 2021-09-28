-- no, I'm not commenting this one either

module Compiler.Parser where

import Text.Parsec
import Text.Parsec.String
import Compiler.Compiler

exprParser :: Parser Expr0
exprParser = try appsParser <|> try lamParser

lamParser = do
  arg <- many1 letter
  many1 space
  string "->"
  many1 space
  expr <- exprParser
  pure $ Lam0 arg expr
subAppsParser = try parenExprParser <|> try varParser
appsParser = do
  (expr0:exprs) <- sepBy subAppsParser (many1 space)
  pure $ foldl App0 expr0 exprs
parenExprParser = do
  char '('
  expr <- exprParser
  char ')'
  pure expr
varParser = do
  var <- many1 letter
  pure $ Var0 var

lineParser :: Parser Line0
lineParser = do
  name <- many1 letter
  many1 space
  char '='
  many1 space
  exp <- exprParser
  spaces
  char ';'
  spaces
  pure $ Line0 name exp

codeParser :: Parser Code0
codeParser = Code0 <$> many1 lineParser

codeFileParser :: Parser Code0
codeFileParser = do
  spaces
  code <- codeParser
  spaces
  eof
  pure code
