{-# LANGUAGE BangPatterns #-}
module GolombEncode where
import Data.Word
import Data.Bits

linewiseDiff :: [Integer] -> [Int]
linewiseDiff ints = zipWith (\ a b -> fromIntegral (a-b) ) ints (0:ints)

fastGolombEncode :: Int -> [Int] -> [Word8]
fastGolombEncode !binaryBits !ints = fGEUnary 7 0 (head ints) binaryBits (tail ints)

fGEUnary :: Int -> Word8 -> Int -> Int -> [Int] -> [Word8]
fGEUnary (-1) !currentWord !currentInt !binaryBits !ints = currentWord : (fGEUnary 7 0 currentInt binaryBits ints)
fGEUnary !bitIndex !currentWord !currentInt !binaryBits !ints | currentInt < (2 ^ binaryBits) = fGEBinary (bitIndex-1) currentWord currentInt (binaryBits-1) binaryBits ints
                                                              | otherwise = fGEUnary (bitIndex-1) (currentWord `setBit` bitIndex) (currentInt - (2 ^ binaryBits)) binaryBits ints

fGEBinary :: Int -> Word8 -> Int -> Int -> Int -> [Int] -> [Word8]
fGEBinary (-1) !currentWord !currentInt !intBitIndex !binaryBits !ints = currentWord : (fGEBinary 7 0 currentInt intBitIndex binaryBits ints)
fGEBinary !bitIndex !currentWord !currentInt (-1) !binaryBits !ints = if null ints then [currentWord] else (fGEUnary bitIndex currentWord (head ints) binaryBits (tail ints))
fGEBinary !bitIndex !currentWord !currentInt !intBitIndex !binaryBits !ints = fGEBinary (bitIndex-1) (if (currentInt `testBit` intBitIndex) then (currentWord `setBit` bitIndex) else currentWord) currentInt (intBitIndex-1) binaryBits ints