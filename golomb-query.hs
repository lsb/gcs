import GolombQuery
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as BWord
import qualified Data.ByteString.Lazy.Char8 as BChar

main = do [lineCount, modulus, binaryBits, gcsFile] <- getArgs  -- todo: use the -index via aeson
          f <- BWord.readFile gcsFile
	  queries <- BChar.getContents
	  BChar.putStr $ BChar.unlines $ golombFilterQueries (read lineCount) (read modulus) (read binaryBits) f (BChar.lines queries)
