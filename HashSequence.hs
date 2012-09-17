module HashSequence where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Set as Set


hashMod :: Integer -> B.ByteString -> Integer
hashMod modulus string = (read $ ("0x" ++) $ show $ md5 string) `mod` modulus

linewiseDiff :: [Integer] -> [Int]
linewiseDiff ints = zipWith (\ a b -> fromIntegral (a-b) ) ints (0:ints)

sortUniq :: [Integer] -> [Integer]
sortUniq = Set.toAscList . Set.fromList
