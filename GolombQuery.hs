module GolombQuery where
import GolombCode
import HashSequence
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B
import Data.List.Ordered (isect)
import Data.List (sort)

linewiseSum :: [Int] -> [Integer]
linewiseSum = scanl ((. fromIntegral) . (+)) 0

golombFilterQueries :: Integer -> Integer -> Int -> [Word8] -> [B.ByteString] -> [B.ByteString]
golombFilterQueries lineCount modulus unaryBits golombCodedSequence queries = filteredQueries
  where sortedHashes = sort [h | (q, h) <- queriesHashes]
        queriesHashes = [(q, hashMod modulus q) | q <- queries]
        filteredHashes = isect unencodedSequence sortedHashes
        filteredQueries = [q | (q,h) <- queriesHashes, h `elem` filteredHashes]
        unencodedSequence = linewiseSum (golombDecodesNoBlocks unaryBits lineCount golombCodedSequence)
