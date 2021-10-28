module Compiler.GenImp where

import Compiler.CodeConversion
import Control.Monad.State
import qualified Data.Set as S

data ImpLine = ImpFnLine Var Var [Var] | ImpCtorLine Var Var Int [Var]
data ImpFn = ImpFn { impFnName :: Var, impArgs :: [Var], impRetVal :: Var, impLines :: [ImpLine] }
data ImpCode = ImpCode { impFns :: [ImpFn], impTypes :: [(Var, Int)] }

getUniqueVarImp :: State (S.Set Var, [ImpLine]) Var
getUniqueVarImp = do
  (oldVars, lines) <- get
  let thisVar = getUnusedVar oldVars
  put (S.singleton thisVar <> oldVars, lines)
  pure thisVar

makeLineImp :: ImpLine -> State (S.Set Var, [ImpLine]) ()
makeLineImp l = modify ((l:) <$>)

convExpr2Imp :: Expr2 -> State (S.Set Var, [ImpLine]) Var
convExpr2Imp a@(App2 _ _) = do
  let exprs = expr2List a
  let lineCtor = makeCtor $ head exprs
  vars <- sequence $ convExpr2Imp <$> tail exprs
  setEqTo <- getUniqueVarImp
  makeLineImp $ lineCtor setEqTo vars
  pure setEqTo
  where
    makeCtor (Var2 s)    a c = ImpFnLine a s c
    makeCtor (Ctor2 n s) a c = ImpCtorLine a s n c
convExpr2Imp (Var2 a) = pure a
convExpr2Imp (Ctor2 0 a) = do
  setEqTo <- getUniqueVarImp
  makeLineImp $ ImpCtorLine setEqTo a 0 []
  pure setEqTo

convLine2Imp :: S.Set Var -> Line2 -> ImpFn
convLine2Imp allVars l = f $ (reverse <$>) <$> runState (convExpr2Imp $ declExpr2 l) (allVars, []) where
  f (retVal,(_,lines)) = ImpFn (declName2 l) (declArgs2 l) retVal lines

convCode2Imp :: Code2 -> ImpCode
convCode2Imp c = ImpCode (convLine2Imp varSet <$> lines2 c) (types2 c) where
  varSet = S.singleton evalFnName <> nameSet <> argsSet
  nameSet = S.fromList $ declName2 <$> lines2 c
  argsSet = S.fromList $ concat $ declArgs2 <$> lines2 c
