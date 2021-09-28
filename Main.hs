import Compiler.CodeConversion
import Compiler.GenHs
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

typePostfix = " | PI Int | PSucc_0 | PAdd_0 | PAdd_1 Monotype | PMul_0 | PMul_1 Monotype"
evalPostfix = "\nveval PSucc_0 (PI n) = (PI (n+1)) \nveval PAdd_0 x = PAdd_1 x \nveval (PAdd_1 (PI x)) (PI y) = PI (x + y) \nveval PMul_0 x = PMul_1 x \nveval (PMul_1 (PI x)) (PI y) = PI (x*y) \ndePI (PI n) = n"
mainFn = "main = print $ dePI $ veval (veval (veval (veval Cmain_0 (PI 0)) PSucc_0) PAdd_0) PMul_0"

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
    callCommand "ghc -no-keep-hi-files -no-keep-o-files otp.hs"
    putStrLn "built successfully"
    callCommand "time ./otp"
    putStrLn "ran successfully"
    callCommand "rm otp.hs"
    callCommand "rm otp"
    putStrLn "cleaned up successfully"
  else print parsed
