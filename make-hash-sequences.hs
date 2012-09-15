import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs)
import HashSequence (processLine)

main = do [modulus] <- getArgs
          B.interact (B.unlines . map (processLine (read modulus)) . B.lines)
