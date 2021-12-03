module Compiler.GenRs where

import Compiler.CodeConversion
import Data.List
import Data.List.Extra
import Data.Char
import qualified Data.Set as S

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

convCtorRs :: S.Set Var -> Ctor4 -> String
convCtorRs vs (Var4 s)
  | S.member s vs = convVarNameRs s <> ".clone()"
  | otherwise = "&" <> convVarNameRs s
convCtorRs vs (Ctor4 s as) = "&new_ctor(Monotype::" <> convCtorNameRs s <> "(" <> intercalate ", " (convCtorRs vs <$> as) <> "))"

convRetCtorRs :: S.Set Var -> Ctor4 -> String
convRetCtorRs vs (Var4 s) = "&*" <> convVarNameRs s <> ".lock().unwrap()"
convRetCtorRs vs (Ctor4 s as) = "Thunk::Ctor(Monotype::" <> convCtorNameRs s <> "(" <> intercalate ", " (convCtorRs vs <$> as) <> "))"

convExprRs :: S.Set Var -> Expr4 -> String
convExprRs vs (CtorExpr4 c) = convRetCtorRs vs c
convExprRs vs (Let4 s a b e) = "let " <> convVarNameRs s <> " = new_eval(" <> convCtorRs vs a <> ", " <> convCtorRs vs b <> ");\n" <> convExprRs vs e

fixBRs :: Var -> String
fixBRs b = "let " <> convVarNameRs b <> " = b;"

evalRs :: Line4 -> String
evalRs l = "Monotype::" <> convCtorNameRs (declCtorName4 l) <> "(" <> intercalate ", " (convVarNameRs <$> declCtorArgs4 l) <> ") => { " <> fixBRs (declArg4 l) <> "\n" <> convExprRs (S.fromList $ declCtorArgs4 l) (declExpr4 l) <> " }"

evalsRs :: Code4 -> String
evalsRs c = unlines $ evalRs <$> lines4 c

ctorDefRs :: (Var, Int) -> String
ctorDefRs (v, n) = convCtorNameRs v <> "(" <> intercalate ", " (replicate n "T") <> "),"

ctorDefsRs :: Code4 -> String
ctorDefsRs c = unlines $ ctorDefRs <$> types4 c

makeMainCtorRs :: String
makeMainCtorRs = "const mainCtor: Monotype<B<Thunk>> = Monotype::" <> convCtorNameRs (conv23CtorName ("main", 0)) <> "();"

replaceInTemplate :: [(String, String)] -> String -> String
replaceInTemplate [] s = s
replaceInTemplate ((a,b):r) s = replaceInTemplate r $ replace ("// [[" <> a <> "]]") b s

convCodeRs :: String -> String -> String -> String -> String -> Code4 -> String
convCodeRs importsPostfix typePostfix evalPostfix mainPostfix codeTemplate c = replaceInTemplate [("EXTRA IMPORTS", importsPostfix), ("TYPE", ctorDefsRs c <> "\n" <> typePostfix), ("EVAL", evalsRs c <> "\n" <> evalPostfix), ("MAIN", makeMainCtorRs <> "\n" <> mainPostfix)] codeTemplate
