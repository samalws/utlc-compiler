import qualified Data.Set as S
import Control.Monad.State
import Data.List

type Var = String

data Expr0 = Lam0 Var Expr0 | App0 Expr0 Expr0 | Var0 Var                       deriving (Show, Eq)
data Line0 = Line0 { declName0 :: Var, declExpr0 :: Expr0 }                     deriving (Show, Eq)
data Code0 = Code0 { lines0 :: [Line0] }                                        deriving (Show, Eq)

data Expr1 = App1 Expr1 Expr1 | Var1 Var                                        deriving (Show, Eq)
data Line1 = Line1 { declName1 :: Var, declArgs1 :: [Var], declExpr1 :: Expr1 } deriving (Show, Eq)
data Code1 = Code1 { lines1 :: [Line1] }                                        deriving (Show, Eq)

data Expr2 = App2 Expr2 Expr2 | Var2 Var | Ctor2 Int Var                        deriving (Show, Eq)
data Line2 = Line2 { declName2 :: Var, declArgs2 :: [Var], declExpr2 :: Expr2 } deriving (Show, Eq)
data Code2 = Code2 { lines2 :: [Line2], types2 :: [(Var, Int)] }                deriving (Show, Eq)

evalFnName :: Var
evalFnName = "eval"

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
  state <- lines1 <$> get
  let lines       = declName1 <$> state
  let args        = join $ declArgs1 <$> state
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
freeVars2 (App2 a b) = freeVars2 a <> freeVars2 b
freeVars2 (Ctor2 _ s) = S.singleton s

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
  pure $ foldr (\var exp -> App1 exp (Var1 var)) (Var1 newName) freeVarsList

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

removeTextStr :: String -> String -> String
removeTextStr s ss
  | (take (length s) ss) == s = s <> ss
  | otherwise = ss

removeTextExpr1 :: String -> Expr1 -> Expr1
removeTextExpr1 s (Var1 a) = Var1 $ removeTextStr s a
removeTextExpr1 s (App1 a b) = App1 (removeTextExpr1 s a) (removeTextExpr1 s b)

removeTextLine1 :: String -> Line1 -> Line1
removeTextLine1 s l = Line1 (removeTextStr s $ declName1 l) (removeTextStr s <$> declArgs1 l) (removeTextExpr1 s $ declExpr1 l)

removeTextCode1 :: String -> Code1 -> Code1
removeTextCode1 s = Code1 . fmap (removeTextLine1 s) . lines1

conv12Expr :: S.Set Var -> Expr1 -> Expr2
conv12Expr env (Var1 s)
  | S.member s env = Var2  s
  | otherwise      = Ctor2 0 s
conv12Expr env (App1 a b) = App2 (App2 (Var2 evalFnName) $ conv12Expr env a) $ conv12Expr env b

conv12Line :: Line1 -> ((Var, Int), Line2)
conv12Line = fixLine . conv12LineHelper where

  fixLine :: ((Var, Int), Line2) -> ((Var, Int), Line2)
  fixLine ((v,0), l) = ((v,1), Line2 (declName2 l) [newVar] apps) where
    apps = App2 (App2 (Var2 evalFnName) (declExpr2 l)) (Var2 newVar)
    newVar = getUnusedVar $ freeVars2 $ declExpr2 l
  fixLine a = a 

  conv12LineHelper :: Line1 -> ((Var, Int), Line2)
  conv12LineHelper l = ((name, length args), Line2 name args $ conv12Expr (S.fromList args) expr) where
    name = declName1 l
    args = declArgs1 l
    expr = declExpr1 l

conv12Code :: Code1 -> Code2
conv12Code = mconcat . fmap (f . conv12Line) . lines1 . removeTextCode1 evalFnName where
  f (a, b) = Code2 [b] [a]

-- bool is whether it's a type / ctor (true) or a variable (false)
convVarHs :: Var -> String
convVarHs = ("v" <>)

convCtorHs :: Int -> Var -> String
convCtorHs n s = "C" <> s <> "_" <> show n

convExpr2Hs :: Expr2 -> String
convExpr2Hs (App2 a b) = "(" <> convExpr2Hs a <> ") (" <> convExpr2Hs b <> ")"
convExpr2Hs (Var2 s)  = convVarHs s
convExpr2Hs (Ctor2 n s) = convCtorHs n s

convLine2Hs :: Line2 -> String
convLine2Hs l = (convVarHs $ declName2 l) <> " " <> args <> " = " <> (convExpr2Hs $ declExpr2 l) where
  args = intercalate " " $ convVarHs <$> declArgs2 l

makeHsTypeDecl2 :: [(Var, Int)] -> String
makeHsTypeDecl2 ctors = "data Monotype = " <> types where
  types = intercalate " | " $ makeCtors <$> ctors
  makeCtors (v, n) = intercalate " | " [makeCtorN v m | m <- [0..n-1]]
  makeCtorN v n = convCtorHs n v <> " " <> intercalate " " (replicate n "Monotype")

makeHsEval2 :: [(Var, Int)] -> String
makeHsEval2 ctors = startText <> (intercalate "\n" $ makeEvals <$> ctors) where
  startText = convVarHs "eval" <> " :: Monotype -> Monotype -> Monotype\n"
  makeEvals (v, n) = intercalate "\n" (makeLastEval v (n-1) : [makeMiddleEval v m | m <- [0..n-2]])
  makeMiddleEval v n = makeEvalGeneric v n $ convCtorHs (n+1) v
  makeLastEval v n = makeEvalGeneric v n $ convVarHs v
  makeEvalGeneric v n fn = convVarHs evalFnName <> " (" <> convCtorHs n v <> " " <> makeVarList n <> ") y = " <> fn <> " " <> makeVarList n <> " y"
  makeVarList n = intercalate " " ["x" <> show m | m <- [1..n]]

convCode2Hs :: String -> String -> String -> Code2 -> String
convCode2Hs typePostfix evalPostfix mainFn c = makeHsTypeDecl2 (types2 c) <> typePostfix <> "\n" <> makeHsEval2 (types2 c) <> evalPostfix <> "\n" <> (intercalate "\n" $ convLine2Hs <$> lines2 c) <> "\n" <> mainFn

exampleCode = Code0 [el1,el2,el3,el4,el5,elM]
el1 = Line0 "zero" $ Lam0 "x" $ Lam0 "y" $ Var0 "y"
el2 = Line0 "succ" $ Lam0 "n" $ Lam0 "x" $ Lam0 "y" $ App0 (App0 (Var0 "n") (Var0 "x")) (App0 (Var0 "x") (Var0 "y"))
el3 = Line0 "add" $ Lam0 "n" $ Lam0 "m" $ App0 (App0 (Var0 "n") (Var0 "succ")) (Var0 "m")
el4 = Line0 "mul" $ Lam0 "n" $ Lam0 "m" $ App0 (App0 (Var0 "n") (App0 (Var0 "add") (Var0 "m"))) (Var0 "zero")
el5 = Line0 "myNumber" $ nApp 1000 (Var0 "succ") (Var0 "zero")
elM = Line0 "main" $ App0 (App0 (Var0 "mul") (Var0 "myNumber")) (Var0 "myNumber")

nApp 0 x y = y
nApp n x y = App0 x (nApp (n-1) x y)

typePostfix = " | PI Int | PSucc_0"
evalPostfix = "\n" <> "veval PSucc_0 (PI n) = (PI (n+1))" <> "\n" <> "dePI (PI n) = n"
mainFn = "main = print $ dePI $ veval (veval Cmain_0 PSucc_0) (PI 0)"

main = putStrLn $ convCode2Hs typePostfix evalPostfix mainFn $ conv12Code $ conv01Code exampleCode
