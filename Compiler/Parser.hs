-- no, I'm not commenting this one either

module Compiler.Parser where

import Text.Parsec
import Text.Parsec.String
import Compiler.Compiler

exprParser :: Parser Expr0
exprParser = try appParser <|> try parenExprParser <|> try lamParser <|> try varParser

parenExprParser = do
  char '('
  expr <- exprParser
  char ')'
  pure expr
lamParser = do
  arg <- many1 letter
  many1 space
  string "->"
  many1 space
  exp <- exprParser
  pure $ Lam0 arg exp
varParser = do
  var <- many1 letter
  pure $ Var0 var
subAppParser = try parenExprParser <|> try lamParser <|> try varParser
appParser = do
  exp1 <- subAppParser
  many1 space
  exp2 <- exprParser
  pure $ App0 exp1 exp2

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
