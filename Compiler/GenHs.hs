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

convCtorHs :: Var -> Int -> String
convCtorHs s n = "C" <> convStrHs s <> "_" <> show n

evalFnNameHs :: Var
evalFnNameHs = "eval"

convExpr2Hs :: Expr2 -> String
convExpr2Hs (Eval2 a b) = "(" <> evalFnNameHs <> " (" <> convExpr2Hs a <> ")) (" <> convExpr2Hs b <> ")"
convExpr2Hs (Var2 s)  = convVarHs s
convExpr2Hs (Ctor2 s n) = convCtorHs s n

convLine2Hs :: Line2 -> String
convLine2Hs l = (convVarHs $ declName2 l) <> " " <> args <> " = " <> (convExpr2Hs $ declExpr2 l) where
  args = intercalate " " $ convVarHs <$> declArgs2 l

makeHsTypeDecl2 :: [(Var, Int)] -> String
makeHsTypeDecl2 ctors = "data Monotype = " <> types where
  types = intercalate " | " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate " | " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v n = convCtorHs v n <> " " <> intercalate " " (replicate n "Monotype")

makeHsEval2 :: [(Var, Int)] -> String
makeHsEval2 ctors = startText <> (intercalate "\n" $ makeEvals <$> ctors) where
  startText = evalFnNameHs <> " :: Monotype -> Monotype -> Monotype\n"
  makeEvals (v, n) = intercalate "\n" (makeLastEval v (n-1) : [makeMiddleEval v m | m <- [0..n-2]])
  makeMiddleEval v n = makeEvalGeneric v n $ convCtorHs v (n+1)
  makeLastEval v n = makeEvalGeneric v n $ convVarHs v
  makeEvalGeneric v n fn = evalFnNameHs <> " (" <> convCtorHs v n <> " " <> makeVarList n <> ") y = " <> fn <> " " <> makeVarList n <> " y"
  makeVarList n = intercalate " " ["x" <> show m | m <- [1..n]]

convCode2Hs :: String -> String -> String -> Code2 -> String
convCode2Hs typePostfix evalPostfix mainFn c = makeHsTypeDecl2 (types2 c) <> typePostfix <> "\n" <> makeHsEval2 (types2 c) <> evalPostfix <> "\n" <> (intercalate "\n" $ convLine2Hs <$> lines2 c) <> "\n" <> mainFn
