module Compiler.GenRs where

import Compiler.CodeConversion
import Compiler.GenImp
import Compiler.GenHs
import Data.List

convVarRs :: Var -> String
convVarRs = convVarHs

convCtorRs :: Int -> Var -> String -- why is this the order?
convCtorRs = convCtorHs

boxedRs :: String -> String
boxedRs v = "Rc::new(" <> v <> ")"

reboxedRs :: String -> String
reboxedRs v = "Rc::clone(&" <> v <> ")"

impLineRs :: ImpLine -> String
impLineRs (ImpFnLine a f vs) = "let " <> convVarRs a <> " = " <> convVarRs f <> "(" <> intercalate ", " (reboxedRs . convVarRs <$> vs) <> ");"
impLineRs (ImpCtorLine a f 0 vs) = "let " <> convVarRs a <> " = " <> boxedRs ("Monotype::" <> convCtorRs 0 f) <> ";"
impLineRs (ImpCtorLine a f n vs) = "let " <> convVarRs a <> " = " <> boxedRs ("Monotype::" <> convCtorRs n f <> "(" <> intercalate ", " (reboxedRs . convVarRs <$> vs) <> ")") <> ";"

impFnRs :: ImpFn -> String
impFnRs f = "fn " <> name <> "(" <> args <> ") -> Rc<Monotype> {\n" <> body <> ret <> "\n}\n" where
  name = convVarRs $ impFnName f
  args = intercalate ", " $ map (<> ": Rc<Monotype>") $ convVarRs <$> impArgs f
  body = mconcat $ map ("  " <>) $ map (<> "\n") $ impLineRs <$> impLines f
  ret = "  " <> convVarRs (impRetVal f)

makeRsEval :: String -> [(Var, Int)] -> String
makeRsEval prefix ctors = startText <> prefix <> "  " <> (intercalate ",\n    " $ makeEvals <$> ctors) <> endText where
  startText = "fn " <> convVarHs evalFnName <> "(a: Rc<Monotype>, y: Rc<Monotype>) -> Rc<Monotype> {\n  match &*a {\n"
  endText = "\n  }\n}\n"
  makeEvals (v, n) = intercalate ",\n    " (makeLastEval v (n-1) : [makeMiddleEval v m | m <- [0..n-2]])
  makeMiddleEval v n = (makeEvalGeneric v n $ "Rc::new(Monotype::" <> convCtorRs (n+1) v) <> ")"
  makeLastEval v n = makeEvalGeneric v n $ convVarRs v
  makeEvalGeneric v 0 fn = "Monotype::" <> convCtorHs 0 v <> " => " <> fn <> "(" <> reboxedRs "y" <> ")"
  makeEvalGeneric v n fn = "Monotype::" <> convCtorHs n v <> "(" <> makeVarList n False <> ") => " <> fn <> "(" <> makeVarList n True <> ", " <> reboxedRs "y" <> ")"
  makeVarList n b = intercalate ", " $ (if b then reboxedRs else id) <$> ["x" <> show m | m <- [1..n]]

makeRsTypeDecl :: String -> [(Var, Int)] -> String
makeRsTypeDecl prefix ctors = "enum Monotype {\n  " <> prefix <> types <> "\n}\n" where
  types = intercalate ",\n  " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate ", " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v 0 = convCtorRs 0 v
  makeCtorN v n = convCtorRs n v <> "(" <> intercalate ", " (replicate n "Rc<Monotype>") <> ")"

impCodeRs :: String -> String -> String -> String -> ImpCode -> String
impCodeRs imports typePrefix evalPrefix mainFn c = "use std::rc::Rc;\n" <> imports <> makeRsTypeDecl typePrefix (impTypes c) <> makeRsEval evalPrefix (impTypes c) <> intercalate "\n" (impFnRs <$> impFns c) <> "\n" <> mainFn
