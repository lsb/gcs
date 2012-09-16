{-# LANGUAGE BangPatterns #-}
module GolombCode where
import Data.List (unfoldr, foldl')
import Data.Word

golombCode :: Int -> Int -> [Bool]
golombCode !modulusBits !integer = encodeUnary quotient ++ encodeBinary modulusBits remainder
  where (quotient, remainder) = integer `divMod` (2 ^ modulusBits)

encodeUnary !quotient = go quotient [False]
  where go 0 bits = bits
        go !n bits = go (n-1) (True : bits)

encodeBinary !modulusBits !integer = reverse $ unfoldr numToBools (modulusBits,integer)
  where numToBools (!mB,!i) = if mB == 0 then Nothing else Just (odd i, (mB-1, i `div` 2))

golombDecode :: Int -> [Bool] -> (Int, [Bool])
golombDecode modulusBits bits = (quotient * (2 ^ modulusBits) + remainder, rest)
  where (!quotient, binaryAndRest) = decodeUnary bits
        (!remainder, rest) = decodeBinary modulusBits binaryAndRest

decodeUnary bits = (length ones, rest)
  where (ones, z : rest) = span id bits

decodeBinary modulusBits bits = (int, rest)
  where (binaryBits, rest) = splitAt modulusBits bits
        int = boolsToInt binaryBits

boolsToInt :: (Num a) => [Bool] -> a
boolsToInt = foldl' (\ z e -> z*2 + (if e then 1 else 0)) 0

boolsToBytes :: [Bool] -> Bool -> [Word8]
boolsToBytes (b7:(b6:(b5:(b4:(b3:(b2:(b1:(b0:nextBytes)))))))) padding = (boolsToInt [b7,b6,b5,b4,b3,b2,b1,b0]) : (boolsToBytes nextBytes padding)
boolsToBytes [] padding = []
boolsToBytes fewBitsShort padding = [boolsToInt $ take 8 $ fewBitsShort ++ repeat padding]

decodeMany 0 modulusBits bits = []
decodeMany !n modulusBits bits = i : (decodeMany (n-1) modulusBits rest)
  where (i,rest) = golombDecode modulusBits bits

golombCodesNoBlocks :: Int -> [Int] -> [Word8]
golombCodesNoBlocks !modulusBits ints = boolsToBytes (concatMap (golombCode modulusBits) ints) False

golombDecodesNoBlocks :: Int -> Int -> [Word8] -> [Int]
golombDecodesNoBlocks !modulusBits !count word8s = decodeMany count modulusBits (concatMap (encodeBinary 8) word8s)

linewiseDiff :: [Integer] -> [Int]
linewiseDiff ints = zipWith (\ a b -> fromIntegral (a-b) ) ints (0:ints)

-- test_golomb = quickCheckWith (stdArgs {maxSuccess = 10000}) (\ ints -> let positives = map abs ints in golombDecodesNoBlocks 2 (fromIntegral $ length ints) (golombCodesNoBlocks 2 positives) == positives)