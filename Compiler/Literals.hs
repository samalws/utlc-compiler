module Compiler.Literals where

import Compiler.CodeConversion
import Data.Char

idLit :: Expr0
idLit = Lam0 "x" $ Var0 "x"

boolLit :: Bool -> Expr0
boolLit b = Lam0 "x" $ Lam0 "y" $ Var0 $ if b then "x" else "y"

-- assumes no free vars in the expr
listLit :: [Expr0] -> Expr0
listLit [] = Lam0 "f" $ Lam0 "x" $ Var0 "x"
listLit (a:b) = Lam0 "f" $ Lam0 "x" $ (Var0 "f" `App0` a) `App0` listLit b

numLit :: Integer -> Expr0
numLit n = listLit $ boolLit <$> numToBoolList n where
  numToBoolList :: Integer -> [Bool]
  numToBoolList 0 = []
  numToBoolList n = (n `mod` 2 == 1) : (numToBoolList $ n `div` 2)

charLit :: Char -> Expr0
charLit c = numLit $ toInteger $ ord c

stringLit :: String -> Expr0
stringLit s = listLit $ charLit <$> s
