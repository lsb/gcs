module HashSequence where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import qualified Text.Show.ByteString as BS (show)

hashMod :: Integer -> B.ByteString -> Integer
hashMod modulus string = (read $ ("0x" ++) $ show $ md5 string) `mod` modulus

processLine modulus = BS.show . hashMod modulus
