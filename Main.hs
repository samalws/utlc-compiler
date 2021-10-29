import Compiler.CodeConversion
import Compiler.GenHs
import Compiler.GenRs
import Compiler.GenImp
import Compiler.Parser
import Text.Parsec
import Data.Either
import System.Process
import Control.Monad.Trans.Either

main = do
  imports    <- readFile "RustAdditions/Imports"
  typePrefix <- readFile "RustAdditions/TypePrefix"
  evalPrefix <- readFile "RustAdditions/EvalPrefix"
  mainFn     <- readFile "RustAdditions/MainFn"

  parsed <- runEitherT $ codeFileParser readFile "main.utlc"
  let frParsed = fromRight (Code0 []) parsed
  let converted = impCodeRs imports typePrefix evalPrefix mainFn $ convCode2Imp $ conv12Code $ conv01Code frParsed
  if (isRight parsed) then do
    writeFile "otp.rs" converted
  else print parsed
