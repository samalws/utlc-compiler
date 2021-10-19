module Compiler.GenImp where

import Compiler.CodeConversion
import Control.Monad.State

-- this is all super tentative

data ImpLine = ImpLine Var Var Var -- a := b(c), but like... that's not good???

makeUniqueVarImp :: State [ImpLine] Var
makeUniqueVarImp = pure "todo"

makeLineImp :: Var -> Var -> State [ImpLine] Var
makeLineImp b c = do
  a <- makeUniqueVarImp
  let thisLine = ImpLine a b c
  modify (thisLine:)
  pure a

convExpr2Imp :: Expr2 -> State [ImpLine] Var
convExpr2Imp (App2 a b) = do
  va <- convExpr2Imp a
  vb <- convExpr2Imp b
  makeLineImp va vb
convExpr2Imp (Var2 a) = pure a
convExpr2Imp (Ctor2 n a) = pure a -- ???

convLine2Imp :: Line2 -> (Var,[ImpLine]) -- Var being the var we return
convLine2Imp l = runState (convExpr2Imp $ declExpr2 l) []
