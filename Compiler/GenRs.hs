module Compiler.GenRs where

import Compiler.CodeConversion
import Compiler.GenImp
import Compiler.GenHs
import Data.List

convVarRs :: Var -> String
convVarRs = convVarHs

convCtorRs :: Int -> Var -> String -- why is this the order?
convCtorRs = convCtorHs

boxedRs :: Var -> String
boxedRs v = "Box::new(" <> v <> ")"

impLineRs :: ImpLine -> String
impLineRs (ImpFnLine a f vs) = "let " <> convVarRs a <> " = " <> convVarRs f <> "(" <> intercalate ", " (convVarRs <$> vs) <> ");"
impLineRs (ImpCtorLine a f 0 vs) = "let " <> convVarRs a <> " = " <> boxedRs ("Monotype::" <> convCtorRs 0 f) <> ";"
impLineRs (ImpCtorLine a f n vs) = "let " <> convVarRs a <> " = " <> boxedRs ("Monotype::" <> convCtorRs n f <> "(" <> intercalate ", " (convVarRs <$> vs) <> ")") <> ";"

impFnRs :: ImpFn -> String
impFnRs f = "fn " <> name <> "(" <> args <> ") -> Box<Monotype> {\n" <> body <> ret <> "\n}\n" where
  name = convVarRs $ impFnName f
  args = intercalate ", " $ map (<> ": Box<Monotype>") $ convVarRs <$> impArgs f
  body = mconcat $ map ("  " <>) $ map (<> "\n") $ impLineRs <$> impLines f
  ret = "  " <> convVarRs (impRetVal f)

makeRsEval :: [(Var, Int)] -> String
makeRsEval ctors = startText <> (intercalate ",\n    " $ makeEvals <$> ctors) <> endText where
  startText = "fn " <> convVarHs evalFnName <> "(a: Box<Monotype>, y: Box<Monotype>) -> Box<Monotype> {\n  match *a {\n    "
  endText = "\n  }\n}\n"
  makeEvals (v, n) = intercalate ",\n    " (makeLastEval v (n-1) : [makeMiddleEval v m | m <- [0..n-2]])
  makeMiddleEval v n = (makeEvalGeneric v n $ "Box::new(Monotype::" <> convCtorRs (n+1) v) <> ")"
  makeLastEval v n = makeEvalGeneric v n $ convVarRs v
  makeEvalGeneric v 0 fn = "Monotype::" <> convCtorHs 0 v <> " => " <> fn <> "(y)"
  makeEvalGeneric v n fn = "Monotype::" <> convCtorHs n v <> "(" <> makeVarList n <> ") => " <> fn <> "(" <> makeVarList n <> ", y)"
  makeVarList n = intercalate ", " ["x" <> show m | m <- [1..n]]

makeRsTypeDecl :: [(Var, Int)] -> String
makeRsTypeDecl ctors = "enum Monotype {\n  " <> types <> "\n}\n" where
  types = intercalate ",\n  " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate ", " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v 0 = convCtorRs 0 v
  makeCtorN v n = convCtorRs n v <> "(" <> intercalate ", " (replicate n "Box<Monotype>") <> ")"

impCodeRs :: ImpCode -> String
impCodeRs c = makeRsTypeDecl (impTypes c) <> makeRsEval (impTypes c) <> intercalate "\n" (impFnRs <$> impFns c)
