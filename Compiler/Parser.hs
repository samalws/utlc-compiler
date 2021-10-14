-- no, I'm not commenting this one either

module Compiler.Parser where

import Compiler.CodeConversion
import Compiler.Literals
import Text.Parsec
import Text.Parsec.String
import Data.Maybe
import Control.Monad.Trans.Class
import Data.Either
import Control.Monad.Trans.Either

data UtlcFile = UtlcFile { utlcFileLines :: [Either (String,String) Line0] }

allowedSpecialChars = "~!@#$%^&*_+|:,<./?->=\\"

commentParser :: Parser ()
commentParser = char '[' >> many (noneOf "]") >> char ']' >> pure ()

whitespace1 :: Parser ()
whitespace1 = many1 (commentParser <|> (space >> pure ())) >> pure ()

whitespace :: Parser ()
whitespace = many (commentParser <|> (space >> pure ())) >> pure ()

varParser :: Parser Var
varParser = do
  var <- many1 $ alphaNum <|> oneOf allowedSpecialChars
  if (var == "->") then fail "Unexpected ->" else return ()
  if (var == "=")  then fail "Unexpected ="  else return ()
  return var

exprParser :: Parser Expr0
exprParser = try appsParser <|> try lamParser

lamParser = do
  arg <- varParser
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
    subAppsParser = try parenExprParser <|> try stringParseParser <|> try varExprParser <|> try stringLitParser
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
stringLitParser = do
  char '"'
  str <- many $ noneOf "\""
  char '"'
  pure $ stringLit str
stringParseParser = do
  p <- varParser
  char '`'
  a <- varParser
  pure $ ((Var0 ("p" <> p) `App0` stringLit a) `App0` idLit) `App0` idLit

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

importParser :: Parser (String,String)
importParser = try importUnqualParser <|> importQualParser

importUnqualParser = do
  char '{'
  s <- many1 $ noneOf "}|"
  char '}'
  whitespace
  pure (s,"")

importQualParser = do
  char '{'
  s1 <- many1 $ noneOf "}|"
  char '|'
  s2 <- many $ noneOf "}|"
  char '}'
  whitespace
  pure (s1,s2)

utlcFileParser :: Parser UtlcFile
utlcFileParser = do
  whitespace
  code <- UtlcFile <$> many1 ((Left <$> importParser) <|> (Right <$> lineParser))
  whitespace
  eof
  pure code

doImport :: (Monad m) => (String -> m String) -> String -> (String,String) -> EitherT ParseError m Code0
doImport readFile ctx (imp,qual) = do
  fileCode <- codeFileParser readFile imp -- todo use ctx to do relative filepaths
  pure $ qualifyLineNames0 qual fileCode

doImports :: (Monad m) => (String -> m String) -> String -> [(String,String)] -> EitherT ParseError m Code0
doImports readFile ctx l = mconcat <$> (sequence $ doImport readFile ctx <$> l)

codeFileParser :: (Monad m) => (String -> m String) -> String -> EitherT ParseError m Code0
codeFileParser readFile fileName = do
  thisFileText <- lift $ readFile fileName
  parsed <- hoistEither $ utlcFileLines <$> parse utlcFileParser fileName thisFileText
  imports <- doImports readFile fileName $ lefts parsed
  pure $ (Code0 $ rights parsed) <> imports
