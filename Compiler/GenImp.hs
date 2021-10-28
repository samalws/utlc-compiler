module Compiler.GenImp where

import Compiler.CodeConversion
import Control.Monad.State

-- todo make vars actually be unique
-- todo do eval

data ImpLine = ImpFnLine Var Var [Var] | ImpCtorLine Var Var [Var]
data ImpFn = ImpFn { impFnName :: Var, impArgs :: [Var], impRetVal :: Var, impLines :: [ImpLine] }
data ImpCode = ImpCode { impFns :: [ImpFn], impTypes :: [(Var, Int)] }

makeUniqueVarImp :: State [ImpLine] Var
makeUniqueVarImp = pure "todo"

makeLineImp :: ImpLine -> State [ImpLine] ()
makeLineImp l = modify (l:)

convExpr2Imp :: Expr2 -> State [ImpLine] Var
convExpr2Imp a@(App2 _ _) = do
  let exprs = expr2List a
  let lineCtor = chooseCtor $ head exprs
  vars <- sequence $ convExpr2Imp <$> exprs
  setEqTo <- makeUniqueVarImp
  makeLineImp $ lineCtor setEqTo (head vars) (tail vars)
  pure setEqTo
  where
    chooseCtor (Var2 _)  = ImpFnLine
    chooseCtor (Ctor2 _ _) = ImpCtorLine
convExpr2Imp (Var2 a) = pure a
convExpr2Imp (Ctor2 n a) = pure a

convLine2Imp :: Line2 -> ImpFn
convLine2Imp l = uncurry (ImpFn (declName2 l) (declArgs2 l)) $ runState (convExpr2Imp $ declExpr2 l) []

convCode2Imp :: Code2 -> ImpCode
convCode2Imp c = ImpCode (convLine2Imp <$> lines2 c) (types2 c)
