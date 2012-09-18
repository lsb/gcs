{-# LANGUAGE BangPatterns #-}
module GolombQuery where
import HashSequence
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy as BW
import Data.List.Ordered (isect)
import Data.List (sort)
import Data.Bits (testBit)

linewiseSum :: [Int] -> [Integer]
linewiseSum = scanl ((. fromIntegral) . (+)) 0

golombFilterQueries :: Integer -> Integer -> Word8 -> BW.ByteString -> [B.ByteString] -> [B.ByteString]
golombFilterQueries lineCount modulus binaryBits golombCodedSequence queries = filteredQueries
  where sortedHashes = sort [h | (q, h) <- queriesHashes]
        queriesHashes = [(q, hashMod modulus q) | q <- queries]
        filteredHashes = isect unencodedSequence sortedHashes
        filteredQueries = [q | (q,h) <- queriesHashes, h `elem` filteredHashes]
        unencodedSequence = linewiseSum (fastGolombDecode lineCount binaryBits golombCodedSequence)

fastGolombDecode :: Integer -> Word8 -> BW.ByteString -> [Int]
fastGolombDecode !lineCount !binaryBits !bytes = fGDUnary (-1) 0 0 lineCount binaryBits bytes

fGDUnary :: Int -> Word8 -> Int -> Integer -> Word8 -> BW.ByteString -> [Int]
fGDUnary !bitIndex !currentWord !currentIntAccum 0 !binaryBytes !bytes = []
fGDUnary (-1) !currentWord !currentIntAccum !intsRemaining !binaryBytes !bytes = fGDUnary 7 (BW.head bytes) currentIntAccum intsRemaining binaryBytes (BW.tail bytes)
fGDUnary !bitIndex !currentWord !currentIntAccum !intsRemaining !binaryBytes !bytes = if currentWord `testBit` bitIndex
                                                                                        then fGDUnary (bitIndex-1) currentWord (currentIntAccum+1) intsRemaining binaryBytes bytes
                                                                                        else fGDBinary (bitIndex-1) currentWord binaryBytes currentIntAccum intsRemaining binaryBytes bytes

fGDBinary :: Int -> Word8 -> Word8 -> Int -> Integer -> Word8 -> BW.ByteString -> [Int]
fGDBinary !bitIndex !currentWord 0 !currentIntAccum !intsRemaining !binaryBytes !bytes = currentIntAccum : (fGDUnary bitIndex currentWord 0 (intsRemaining-1) binaryBytes bytes)
fGDBinary (-1) !currentWord !intBitsRemaining !currentIntAccum !intsRemaining !binaryBytes !bytes = fGDBinary 7 (BW.head bytes) intBitsRemaining currentIntAccum intsRemaining binaryBytes (BW.tail bytes)
fGDBinary !bitIndex !currentWord !intBitsRemaining !currentIntAccum !intsRemaining !binaryBytes !bytes = fGDBinary (bitIndex-1) currentWord (intBitsRemaining-1) (2*currentIntAccum + (if currentWord `testBit` bitIndex then 1 else 0)) intsRemaining binaryBytes bytes
