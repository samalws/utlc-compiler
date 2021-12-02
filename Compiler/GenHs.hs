module Compiler.GenHs where

import Compiler.CodeConversion
import Data.List
import Data.Char

convCharHs :: Char -> String
convCharHs 'x' = "xx" -- escape character
convCharHs c
  | isAlphaNum c = pure c
  | otherwise  = "x" <> show (ord c) <> "x"

convStrHs :: String -> String
convStrHs = mconcat . (convCharHs <$>)

convVarHs :: Var -> String
convVarHs = ("v" <>) . convStrHs

convCtorHs :: Var -> String
convCtorHs s = "C" <> convStrHs s

evalFnNameHs :: Var
evalFnNameHs = "eval"

convCtor4Hs :: Ctor4 -> String
convCtor4Hs (Var4 s) = convVarHs s
convCtor4Hs (Ctor4 s as) = "(" <> unwords ((convCtorHs s):(convCtor4Hs <$> as)) <> ")"

convExpr4Hs :: Expr4 -> String
convExpr4Hs (CtorExpr4 c) = convCtor4Hs c
convExpr4Hs (Let4 s a b e) = "let " <> convVarHs s <> " = " <> evalFnNameHs <> " " <> convCtor4Hs a <> " " <> convCtor4Hs b <> " in " <> convExpr4Hs e

convLine4Hs :: Line4 -> String
convLine4Hs l = evalFnNameHs <> " (" <> unwords ((convCtorHs (declCtorName4 l)) : (convVarHs <$> declCtorArgs4 l)) <> ") " <> convVarHs (declArg4 l) <> " = " <> convExpr4Hs (declExpr4 l)

makeHsTypeDecl4 :: [(Var, Int)] -> String
makeHsTypeDecl4 ctors = "data Monotype = " <> types where
  types = intercalate " | " $ makeCtor <$> ctors
  makeCtor (v, n) = convCtorHs v <> " " <> unwords (replicate n "Monotype")

makeMainCtor :: String
makeMainCtor = "mainCtor :: Monotype\nmainCtor = " <> convCtorHs (conv23CtorName ("main", 0))

convCode4Hs :: String -> String -> String -> Code4 -> String
convCode4Hs typePostfix evalPostfix mainFn c = makeHsTypeDecl4 (types4 c) <> typePostfix <> "\n" <> unlines (convLine4Hs <$> lines4 c) <> evalPostfix <> "\n" <> makeMainCtor <> "\n" <> mainFn
