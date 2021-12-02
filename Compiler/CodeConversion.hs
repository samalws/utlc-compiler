-- no, I'm not going to comment it yet

module Compiler.CodeConversion where

import qualified Data.Set as S
import Control.Monad.State
import Data.List

type Var = String

-- base code, entered by the user
data Expr0 = Lam0 Var Expr0 | App0 Expr0 Expr0 | Var0 Var                       deriving (Show, Eq)
data Line0 = Line0 { declName0 :: Var, declExpr0 :: Expr0 }                     deriving (Show, Eq)
data Code0 = Code0 { lines0 :: [Line0] }                                        deriving (Show, Eq)

-- lambda lifted code
data Expr1 = App1 Expr1 Expr1 | Var1 Var                                        deriving (Show, Eq)
data Line1 = Line1 { declName1 :: Var, declArgs1 :: [Var], declExpr1 :: Expr1 } deriving (Show, Eq)
data Code1 = Code1 { lines1 :: [Line1] }                                        deriving (Show, Eq)

-- defunctionalized code
data Expr2 = Eval2 Expr2 Expr2 | Var2 Var | Ctor2 Var Int                       deriving (Show, Eq)
data Line2 = Line2 { declName2 :: Var, declArgs2 :: [Var], declExpr2 :: Expr2 } deriving (Show, Eq)
data Code2 = Code2 { lines2 :: [Line2], types2 :: [(Var, Int)] }                deriving (Show, Eq)

-- code with only 1 recursive function, eval
-- each line means "eval (F a b) c = ..."
data Expr3 = Eval3 Expr3 Expr3 | Var3 Var | Ctor3 Var [Expr3]                                            deriving (Show, Eq)
data Line3 = Line3 { declCtorName3 :: Var, declCtorArgs3 :: [Var], declArg3 :: Var, declExpr3 :: Expr3 } deriving (Show, Eq)
data Code3 = Code3 { lines3 :: [Line3], types3 :: [Var] }                                                deriving (Show, Eq)

-- A normal form
data Ctor4 = Var4 Var | Ctor4 Var [Ctor4]                                                                deriving (Show, Eq)
data Expr4 = Let4 Var Ctor4 Ctor4 Expr4 | CtorExpr4 Ctor4                                                deriving (Show, Eq)
data Line4 = Line4 { declCtorName4 :: Var, declCtorArgs4 :: [Var], declArg4 :: Var, declExpr4 :: Expr4 } deriving (Show, Eq)
data Code4 = Code4 { lines4 :: [Line4], types4 :: [Var] }                                                deriving (Show, Eq)

instance Semigroup Code0 where
  a <> b = Code0 $ lines0 a <> lines0 b
instance Monoid Code0 where
  mempty = Code0 []

instance Semigroup Code1 where
  a <> b = Code1 $ lines1 a <> lines1 b
instance Monoid Code1 where
  mempty = Code1 []

instance Semigroup Code2 where
  a <> b = Code2 (lines2 a <> lines2 b) (types2 a <> types2 b)
instance Monoid Code2 where
  mempty = Code2 [] []

getUnusedVar :: S.Set Var -> Var
getUnusedVar s = head $ filter (not . flip S.member s) $ iterate ("x" <>) "x"

getUnusedVar1 :: S.Set Var -> State Code1 Var
getUnusedVar1 set = do
  state <- gets lines1
  let lines       = declName1 <$> state
  let args        = declArgs1 =<< state
  let setFromCode = S.fromList $ lines <> args
  let fullSet     = set <> setFromCode
  pure $ getUnusedVar fullSet

addToCode1 :: Line1 -> State Code1 ()
addToCode1 a = modify $ Code1 . (a:) . lines1

freeVars0 :: Expr0 -> S.Set Var
freeVars0 (Var0 s) = S.singleton s
freeVars0 (App0 a b) = freeVars0 a <> freeVars0 b
freeVars0 (Lam0 s a) = S.delete s $ freeVars0 a

freeVars2 :: Expr2 -> S.Set Var
freeVars2 (Var2 s) = S.singleton s
freeVars2 (Eval2 a b) = freeVars2 a <> freeVars2 b
freeVars2 (Ctor2 s _) = S.singleton s

conv01Expr :: S.Set Var -> Expr0 -> State Code1 ([Var], Expr1)
conv01Expr = conv01ExprStart S.empty

-- v1: vars that we've accumulated from lambdas
-- v2: vars that we should avoid because they're present in Code0 decls
conv01ExprStart :: S.Set Var -> S.Set Var -> Expr0 -> State Code1 ([Var], Expr1)
conv01ExprStart v1 v2 (Lam0 s a) = do
  (v,b) <- conv01ExprStart (S.insert s v1) v2 a
  pure (s:v, b)
conv01ExprStart v1 v2 a = do
  ca <- conv01ExprEnd v1 v2 a
  pure ([], ca)

conv01ExprEnd :: S.Set Var -> S.Set Var -> Expr0 -> State Code1 Expr1
conv01ExprEnd v1 v2 (Var0 s) = pure $ Var1 s
conv01ExprEnd v1 v2 (App0 a b) = do
  ca <- conv01ExprEnd v1 v2 a
  cb <- conv01ExprEnd v1 v2 b
  pure $ App1 ca cb
conv01ExprEnd v1 v2 a@(Lam0 _ _) = do
  let freeVars = freeVars0 a
  let freeVarsList = S.toList freeVars
  (lamVarsList,convedA) <- conv01Expr v2 a
  newName <- getUnusedVar1 $ v1 <> v2 <> freeVars <> S.fromList lamVarsList
  addToCode1 $ Line1 newName (freeVarsList <> lamVarsList) convedA
  pure $ foldl (\exp var -> App1 exp (Var1 var)) (Var1 newName) freeVarsList

conv01Line :: S.Set Var -> Line0 -> State Code1 Line1
conv01Line v2 (Line0 s a) = do
  (l,b) <- conv01Expr v2 a
  pure $ Line1 s l b

conv01Code :: Code0 -> Code1
conv01Code c0 = Code1 a <> b where
  l0  = lines0 c0
  v2  = S.fromList $ declName0 <$> l0
  l1s = sequence $ conv01Line v2 <$> l0
  (a,b) = runState l1s $ Code1 []

conv12Expr :: S.Set Var -> Expr1 -> Expr2
conv12Expr env (Var1 s)
  | S.member s env = Var2  s
  | otherwise      = Ctor2 s 0
conv12Expr env (App1 a b) = Eval2 (conv12Expr env a) (conv12Expr env b)

conv12Line :: Line1 -> ((Var, Int), Line2)
conv12Line = fixLine . conv12LineHelper where
  fixLine :: ((Var, Int), Line2) -> ((Var, Int), Line2)
  fixLine ((v,0), l) = ((v,1), Line2 (declName2 l) [newVar] apps) where
    apps = Eval2 (declExpr2 l) (Var2 newVar)
    newVar = getUnusedVar $ freeVars2 $ declExpr2 l
  fixLine a = a 

  conv12LineHelper :: Line1 -> ((Var, Int), Line2)
  conv12LineHelper l = ((name, length args), Line2 name args $ conv12Expr (S.fromList args) expr) where
    name = declName1 l
    args = declArgs1 l
    expr = declExpr1 l

conv12Code :: Code1 -> Code2
conv12Code = mconcat . fmap (f . conv12Line) . lines1 where
  f (a, b) = Code2 [b] [a]

conv23CtorName :: (Var, Int) -> Var
conv23CtorName (a, n) = show n <> "_" <> a

conv23Expr :: Expr2 -> Expr3
conv23Expr (Eval2 a b) = Eval3 (conv23Expr a) (conv23Expr b)
conv23Expr (Var2 s) = Var3 s
conv23Expr (Ctor2 s n) = Ctor3 (conv23CtorName (s, n)) []

conv23Line :: Line2 -> [Line3]
conv23Line l = lastLine:preLines where
  n = length (declArgs2 l)
  lastLine = Line3 { declCtorName3 = conv23CtorName (declName2 l, n-1),
                     declCtorArgs3 = init (declArgs2 l),
                     declArg3 = last (declArgs2 l),
                     declExpr3 = conv23Expr (declExpr2 l) }
  preLines = f <$> [0..n-2]
  f m = Line3 { declCtorName3 = conv23CtorName (declName2 l, m),
                declCtorArgs3 = args,
                declArg3 = "y",
                declExpr3 = Ctor3 (conv23CtorName (declName2 l, m+1)) (Var3 <$> (args <> ["y"])) }
          where args = ["x" <> show a | a <- [0..(m-1)]]

conv23Types :: (Var, Int) -> [Var]
conv23Types (s, 0) = [conv23CtorName (s, 0)]
conv23Types (s, n) = (conv23CtorName (s, n)):(conv23Types (s, n-1))

conv23Code :: Code2 -> Code3
conv23Code c = Code3 { lines3 = concat (conv23Line <$> lines2 c), types3 = concat (conv23Types <$> types2 c) }

getUniqueVar4 :: State (S.Set Var, [(Var, Ctor4, Ctor4)]) Var
getUniqueVar4 = do
  (oldVars, lets) <- get
  let thisVar = getUnusedVar oldVars
  put (S.singleton thisVar <> oldVars, lets)
  pure thisVar

makeLet4 :: (Var, Ctor4, Ctor4) -> State (S.Set Var, [(Var, Ctor4, Ctor4)]) ()
makeLet4 l = modify ((l:) <$>)

conv34ExprLets :: Expr3 -> State (S.Set Var, [(Var, Ctor4, Ctor4)]) Ctor4
conv34ExprLets (Eval3 a b) = do
  va <- conv34ExprLets a
  vb <- conv34ExprLets b
  setEqTo <- getUniqueVar4
  makeLet4 (setEqTo, va, vb)
  pure $ Var4 setEqTo
conv34ExprLets (Var3 a) = pure $ Var4 a
conv34ExprLets (Ctor3 a bs) = do
  vs <- sequence $ conv34ExprLets <$> bs
  setEqTo <- getUniqueVar4
  pure $ Ctor4 a vs

mergeLets4 :: [(Var, Ctor4, Ctor4)] -> Ctor4 -> Expr4
mergeLets4 [] ret = CtorExpr4 ret
mergeLets4 ((a,b,c):d) ret = Let4 a b c $ mergeLets4 d ret

conv34Expr :: S.Set Var -> Expr3 -> Expr4
conv34Expr allVars a = f $ (reverse <$>) <$> runState (conv34ExprLets a) (allVars, []) where
  f (retVal, (_, lets)) = mergeLets4 lets retVal

conv34Line :: S.Set Var -> Line3 -> Line4
conv34Line allVars l = Line4 (declCtorName3 l) (declCtorArgs3 l) (declArg3 l) (conv34Expr allVars $ declExpr3 l)

conv34Code :: Code3 -> Code4
conv34Code c = Code4 (conv34Line varSet <$> lines3 c) (types3 c) where
  varSet = nameSet <> ctorArgsSet <> argSet
  nameSet = S.fromList $ declCtorName3 <$> lines3 c
  ctorArgsSet = S.fromList $ concat $ declCtorArgs3 <$> lines3 c
  argSet = S.fromList $ declArg3 <$> lines3 c
