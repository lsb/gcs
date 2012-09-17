import qualified Data.ByteString.Lazy.Char8 as B
import qualified Text.Show.ByteString as BS (show)
import System.Environment (getArgs)
import HashSequence

main = do [modulus] <- getArgs
          B.interact (B.unlines . map BS.show . linewiseDiff . sortUniq . map (hashMod (read modulus)) . B.lines)
