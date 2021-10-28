module Compiler.GenImp where

import Compiler.CodeConversion
import Control.Monad.State

-- this is all super tentative

data ImpLine = ImpFnLine Var [Var] | ImpCtorLine Var [Var]
data ImpFn = ImpFn Var [ImpLine]
data ImpCode = ImpCode [ImpFn]

makeUniqueVarImp :: State [ImpLine] Var
makeUniqueVarImp = pure "todo"

makeLineImp :: ImpLine -> State [ImpLine] ()
makeLineImp l = modify (l:)

convExpr2Imp :: Expr2 -> State [ImpLine] Var
convExpr2Imp a@(App2 _ _) = do
  let exprs = expr2List a
  let lineCtor = chooseCtor $ head exprs
  vars <- sequence $ convExpr2Imp <$> exprs
  makeLineImp $ lineCtor (head vars) (tail vars)
  pure $ head vars
  where
    chooseCtor (Var2 _)  = ImpFnLine
    chooseCtor (Ctor2 _ _) = ImpCtorLine
convExpr2Imp (Var2 a) = pure a
convExpr2Imp (Ctor2 n a) = pure a

convLine2Imp :: Line2 -> ImpFn
convLine2Imp l = uncurry ImpFn $ runState (convExpr2Imp $ declExpr2 l) []

convCode2Imp :: Code2 -> ImpCode
convCode2Imp c = ImpCode $ convLine2Imp <$> lines2 c
