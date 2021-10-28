module Compiler.GenRs where

import Compiler.CodeConversion
import Compiler.GenImp
import Compiler.GenHs
import Data.List

convStrRs :: Var -> String
convStrRs = convVarHs

convCtorRs :: Int -> Var -> String -- why is this the order?
convCtorRs = convCtorHs

boxedRs :: Var -> String
boxedRs v = "Box::new(" <> v <> ")"

impLineRs :: ImpLine -> String
impLineRs (ImpFnLine a f vs) = a <> ":= " <> f <> "(" <> intercalate ", " (boxedRs <$> vs) <> ");"
impLineRs (ImpCtorLine a f vs) = impLineRs (ImpFnLine a f vs)

impFnRs :: ImpFn -> String
impFnRs f = "fn " <> name <> "(" <> args <> ") -> Box<Monotype> {\n" <> body <> ret <> "\n}\n" where
  name = impFnName f
  args = intercalate ", " $ map (<> ": Box<Monotype>") $ impArgs f
  body = mconcat $ map ("  " <>) $ map (<> "\n") $ impLineRs <$> impLines f
  ret = "  return " <> boxedRs (impRetVal f) <> ";"

makeRsTypeDecl :: [(Var, Int)] -> String
makeRsTypeDecl ctors = "enum Monotype {\n  " <> types <> "\n}\n" where
  types = intercalate ",\n  " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate ", " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v n = convCtorRs n v <> "(" <> intercalate ", " (replicate n "Box<Monotype>") <> ")"

impCodeRs :: ImpCode -> String
impCodeRs c = makeRsTypeDecl (impTypes c) <> intercalate "\n" (impFnRs <$> impFns c)
