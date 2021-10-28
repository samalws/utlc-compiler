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

convCtorHs :: Int -> Var -> String
convCtorHs n s = "C" <> convStrHs s <> "_" <> show n

convExpr2Hs :: Expr2 -> String
convExpr2Hs (App2 a b) = "(" <> convExpr2Hs a <> ") (" <> convExpr2Hs b <> ")"
convExpr2Hs (Var2 s)  = convVarHs s
convExpr2Hs (Ctor2 n s) = convCtorHs n s

convLine2Hs :: Line2 -> String
convLine2Hs l = (convVarHs $ declName2 l) <> " " <> args <> " = " <> (convExpr2Hs $ declExpr2 l) where
  args = intercalate " " $ convVarHs <$> declArgs2 l

makeHsTypeDecl :: [(Var, Int)] -> String
makeHsTypeDecl ctors = "data Monotype = " <> types where
  types = intercalate " | " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate " | " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v n = convCtorHs n v <> " " <> intercalate " " (replicate n "Monotype")

makeHsEval :: [(Var, Int)] -> String
makeHsEval ctors = startText <> (intercalate "\n" $ makeEvals <$> ctors) where
  startText = convVarHs "eval" <> " :: Monotype -> Monotype -> Monotype\n"
  makeEvals (v, n) = intercalate "\n" (makeLastEval v (n-1) : [makeMiddleEval v m | m <- [0..n-2]])
  makeMiddleEval v n = makeEvalGeneric v n $ convCtorHs (n+1) v
  makeLastEval v n = makeEvalGeneric v n $ convVarHs v
  makeEvalGeneric v n fn = convVarHs evalFnName <> " (" <> convCtorHs n v <> " " <> makeVarList n <> ") y = " <> fn <> " " <> makeVarList n <> " y"
  makeVarList n = intercalate " " ["x" <> show m | m <- [1..n]]

convCode2Hs :: String -> String -> String -> Code2 -> String
convCode2Hs typePostfix evalPostfix mainFn c = makeHsTypeDecl (types2 c) <> typePostfix <> "\n" <> makeHsEval (types2 c) <> evalPostfix <> "\n" <> (intercalate "\n" $ convLine2Hs <$> lines2 c) <> "\n" <> mainFn
