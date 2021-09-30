import Compiler.CodeConversion
import Compiler.GenHs
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

main = do
  typePostfix <- readFile "Additions/TypePostfix"
  evalPostfix <- readFile "Additions/EvalPostfix"
  mainFn      <- readFile "Additions/MainFn"

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
