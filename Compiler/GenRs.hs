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
  ret = "  return " <> convVarRs (impRetVal f) <> ";"

makeRsTypeDecl :: [(Var, Int)] -> String
makeRsTypeDecl ctors = "enum Monotype {\n  " <> types <> "\n}\n" where
  types = intercalate ",\n  " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate ", " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v 0 = convCtorRs 0 v
  makeCtorN v n = convCtorRs n v <> "(" <> intercalate ", " (replicate n "Box<Monotype>") <> ")"

impCodeRs :: ImpCode -> String
impCodeRs c = makeRsTypeDecl (impTypes c) <> intercalate "\n" (impFnRs <$> impFns c)
