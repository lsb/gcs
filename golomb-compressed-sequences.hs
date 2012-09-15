import GolombCode
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

main = do [modulusBits] <- getArgs
          B.interact (B.pack . map (toEnum . fromEnum) . golombCodesNoBlocks (read modulusBits) . linewiseDiff . map (fst . fromJust . B.readInteger) . B.lines)
