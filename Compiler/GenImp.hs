module Compiler.GenImp where

import Compiler.CodeConversion
import Control.Monad.State

-- todo make vars actually be unique
-- todo do eval

data ImpLine = ImpFnLine Var Var [Var] | ImpCtorLine Var Var Int [Var]
data ImpFn = ImpFn { impFnName :: Var, impArgs :: [Var], impRetVal :: Var, impLines :: [ImpLine] }
data ImpCode = ImpCode { impFns :: [ImpFn], impTypes :: [(Var, Int)] }

makeUniqueVarImp :: State [ImpLine] Var
makeUniqueVarImp = pure "todo"

makeLineImp :: ImpLine -> State [ImpLine] ()
makeLineImp l = modify (l:)

convExpr2Imp :: Expr2 -> State [ImpLine] Var
convExpr2Imp a@(App2 _ _) = do
  let exprs = expr2List a
  let lineCtor = makeCtor $ head exprs
  vars <- sequence $ convExpr2Imp <$> tail exprs
  setEqTo <- makeUniqueVarImp
  makeLineImp $ lineCtor setEqTo vars
  pure setEqTo
  where
    makeCtor (Var2 s)    a c = ImpFnLine a s c
    makeCtor (Ctor2 n s) a c = ImpCtorLine a s n c
convExpr2Imp (Var2 a) = pure a
convExpr2Imp (Ctor2 0 a) = do
  setEqTo <- makeUniqueVarImp
  makeLineImp $ ImpCtorLine setEqTo a 0 []
  pure setEqTo

convLine2Imp :: Line2 -> ImpFn
convLine2Imp l = uncurry (ImpFn (declName2 l) (declArgs2 l)) $ reverse <$> runState (convExpr2Imp $ declExpr2 l) []

convCode2Imp :: Code2 -> ImpCode
convCode2Imp c = ImpCode (convLine2Imp <$> lines2 c) (types2 c)
