import Compiler.CodeConversion
import Compiler.GenHs
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

main = do
  imports     <- readFile "Additions/Imports"
  typePostfix <- readFile "Additions/TypePostfix"
  evalPostfix <- readFile "Additions/EvalPostfix"
  mainFn      <- readFile "Additions/MainFn"

  text <- readFile "main.utlc"
  let parsed = parse codeFileParser "main.utlc" text
  let frParsed = fromRight (Code0 []) parsed
  let converted = imports <> (convCode2Hs typePostfix evalPostfix mainFn $ conv12Code $ conv01Code frParsed) -- TODO jank
  if (isRight parsed) then do
    writeFile "otp.hs" converted
    callCommand "ghc -no-keep-hi-files -no-keep-o-files otp.hs"
    callCommand "./otp"
    callCommand "rm otp.hs"
    callCommand "rm otp"
  else print parsed
