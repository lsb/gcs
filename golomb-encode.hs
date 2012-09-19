import GolombEncode
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)

main = do [binaryBits] <- getArgs
          B.interact (B.pack . map (toEnum . fromEnum) . fastGolombEncode (read binaryBits) . linewiseDiff . map (fst . fromJust . B.readInteger) . B.lines)
