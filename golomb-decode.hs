import GolombQuery
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BWord

main = do [lineCount, binaryBits] <- getArgs  -- todo: use the -index via aeson
          f <- BWord.getContents
	  putStr $ unlines $ map show $ fastGolombDecode (read lineCount) (read binaryBits) f
