import Compiler.Compiler
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

typePostfix = " | PI Int | PSucc_0"
evalPostfix = "\n" <> "veval PSucc_0 (PI n) = (PI (n+1))" <> "\n" <> "dePI (PI n) = n"
mainFn = "main = print $ dePI $ veval (veval Cmain_0 PSucc_0) (PI 0)"

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
