import Compiler.Compiler
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

{-
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
-}

typePostfix = " | PI Int | PSucc_0"
evalPostfix = "\n" <> "veval PSucc_0 (PI n) = (PI (n+1))" <> "\n" <> "dePI (PI n) = n"
mainFn = "main = print $ dePI $ veval (veval Cmain_0 PSucc_0) (PI 0)"

-- main = putStrLn $ convCode2Hs typePostfix evalPostfix mainFn $ conv12Code $ conv01Code exampleCode


main = do
  text <- readFile "main.utlc"
  putStrLn text
  let parsed = parse codeFileParser "main.utlc" text
  let frParsed = fromRight (Code0 []) parsed
  let converted = convCode2Hs typePostfix evalPostfix mainFn $ conv12Code $ conv01Code frParsed
  if (isRight parsed) then do
    putStrLn converted
    writeFile "otp.hs" converted
    putStrLn "outputted to otp.hs"
    callCommand "ghc otp.hs"
    putStrLn "built successfully"
    callCommand "./otp"
    putStrLn "ran successfully"
  else print parsed
