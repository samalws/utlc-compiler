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
  parsed <- runEitherT $ codeFileParser readFile "main.utlc"
  let frParsed = fromRight (Code0 []) parsed
  let converted = impCodeRs $ convCode2Imp $ conv12Code $ conv01Code frParsed
  if (isRight parsed) then do
    writeFile "otp.rs" $ converted <> "\nfn main(){}\nfn veval(a: Box<Monotype>, b: Box<Monotype>) -> Box<Monotype> {\n  return Box::new(Monotype::Cid_0)\n}"
  else print parsed
