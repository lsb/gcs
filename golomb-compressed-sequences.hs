import GolombCode
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Debug.Trace (traceShow)

main = do [modulusBits] <- getArgs
          B.interact (B.pack . map (toEnum . fromEnum) . golombCodesNoBlocks (read modulusBits) . map (fst . fromJust . B.readInt) . B.lines)
