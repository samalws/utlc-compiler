import Compiler.CodeConversion
import Compiler.GenRs
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process

main = do
  imports     <- readFile "RustAdditions/Imports"
  typePostfix <- readFile "RustAdditions/TypePostfix"
  evalPostfix <- readFile "RustAdditions/EvalPostfix"
  mainFn      <- readFile "RustAdditions/MainFn"
  template    <- readFile "RustTemplate"

  text <- readFile "main.utlc"
  let parsed = parse codeFileParser "main.utlc" text
  let frParsed = fromRight (Code0 []) parsed
  let converted = convCodeRs imports typePostfix evalPostfix mainFn template $ conv34Code $ conv23Code $ conv12Code $ conv01Code frParsed
  if (isRight parsed) then do
    writeFile "otp.rs" converted
    callCommand "rustc otp.rs"
    callCommand "./otp"
    callCommand "rm otp.rs"
    callCommand "rm otp"
  else print parsed
