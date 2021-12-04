module Compiler.GenRs where

import Compiler.CodeConversion
import Data.List
import Data.List.Extra
import Data.Char

convCharRs :: Char -> String
convCharRs 'x' = "xx" -- escape character
convCharRs c
  | isAlphaNum c = pure c
  | otherwise  = "x" <> show (ord c) <> "x"

convStrRs :: String -> String
convStrRs = mconcat . (convCharRs <$>)

convVarNameRs :: Var -> String
convVarNameRs = ("v" <>) . convStrRs

convCtorNameRs :: Var -> String
convCtorNameRs s = "C" <> convStrRs s

convCtorRs :: Ctor4 -> String
convCtorRs (Var4 s) = convVarNameRs s <> ".clone()"
convCtorRs (Ctor4 s as) = "new_ctor(Monotype::" <> convCtorNameRs s <> "(" <> intercalate ", " (convCtorRs <$> as) <> "))"

convRetCtorRs :: Ctor4 -> String
convRetCtorRs (Var4 s) = "let x = " <> convVarNameRs s <> ".lock().unwrap().clone(); Some(x)"
convRetCtorRs (Ctor4 s as) = "Some(Thunk::Ctor(Monotype::" <> convCtorNameRs s <> "(" <> intercalate ", " (convCtorRs <$> as) <> ")))"

convExprRs :: Expr4 -> String
convExprRs (CtorExpr4 c) = convRetCtorRs c
convExprRs (Let4 s a b e) = "let " <> convVarNameRs s <> " = new_eval(" <> convCtorRs a <> ", " <> convCtorRs b <> ");\n" <> convExprRs e

fixBRs :: Var -> String
fixBRs b = "let " <> convVarNameRs b <> " = b;"

evalRs :: Line4 -> String
evalRs l = "Monotype::" <> convCtorNameRs (declCtorName4 l) <> "(" <> intercalate ", " (convVarNameRs <$> declCtorArgs4 l) <> ") => { " <> fixBRs (declArg4 l) <> "\n" <> convExprRs (declExpr4 l) <> " }"

evalsRs :: Code4 -> String
evalsRs c = unlines $ evalRs <$> lines4 c

ctorDefRs :: (Var, Int) -> String
ctorDefRs (v, n) = convCtorNameRs v <> "(" <> intercalate ", " (replicate n "T") <> "),"

ctorDefsRs :: Code4 -> String
ctorDefsRs c = unlines $ ctorDefRs <$> types4 c

makeMainCtorRs :: String
makeMainCtorRs = "const MAIN_CTOR: Monotype<B<Thunk>> = Monotype::" <> convCtorNameRs (conv23CtorName ("main", 0)) <> "();"

replaceInTemplate :: [(String, String)] -> String -> String
replaceInTemplate [] s = s
replaceInTemplate ((a,b):r) s = replaceInTemplate r $ replace ("// [[" <> a <> "]]") b s

convCodeRs :: String -> String -> String -> String -> String -> Code4 -> String
convCodeRs importsPostfix typePostfix evalPostfix mainPostfix codeTemplate c = replaceInTemplate [("EXTRA IMPORTS", importsPostfix), ("TYPE", ctorDefsRs c <> "\n" <> typePostfix), ("EVAL", evalsRs c <> "\n" <> evalPostfix), ("MAIN", makeMainCtorRs <> "\n" <> mainPostfix)] codeTemplate
