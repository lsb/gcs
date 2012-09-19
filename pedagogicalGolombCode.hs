{-# LANGUAGE BangPatterns #-}
module GolombCode where
import Data.List (unfoldr, foldl')
import Data.Word

golombCode :: Int -> Int -> [Bool]
golombCode !binaryBits !integer = encodeUnary quotient ++ encodeBinary binaryBits remainder
  where (quotient, remainder) = integer `divMod` (2 ^ binaryBits)

encodeUnary !quotient = go quotient [False]
  where go 0 bits = bits
        go !n bits = go (n-1) (True : bits)

encodeBinary !binaryBits !integer = reverse $ unfoldr numToBools (binaryBits,integer)
  where numToBools (!mB,!i) = if mB == 0 then Nothing else Just (odd i, (mB-1, i `div` 2))

golombDecode :: Int -> [Bool] -> (Int, [Bool])
golombDecode binaryBits bits = (quotient * (2 ^ binaryBits) + remainder, rest)
  where (!quotient, binaryAndRest) = decodeUnary bits
        (!remainder, rest) = decodeBinary binaryBits binaryAndRest

decodeUnary bits = (length ones, rest)
  where (ones, z : rest) = span id bits

decodeBinary binaryBits bits = (int, rest)
  where (binary, rest) = splitAt binaryBits bits
        int = boolsToInt binary

boolsToInt :: (Num a) => [Bool] -> a
boolsToInt = foldl' (\ z e -> z*2 + (if e then 1 else 0)) 0

boolsToBytes :: [Bool] -> Bool -> [Word8]
boolsToBytes (b7:(b6:(b5:(b4:(b3:(b2:(b1:(b0:nextBytes)))))))) padding = (boolsToInt [b7,b6,b5,b4,b3,b2,b1,b0]) : (boolsToBytes nextBytes padding)
boolsToBytes [] padding = []
boolsToBytes fewBitsShort padding = [boolsToInt $ take 8 $ fewBitsShort ++ repeat padding]

decodeMany 0 binaryBits bits = []
decodeMany !n binaryBits bits = i : (decodeMany (n-1) binaryBits rest)
  where (i,rest) = golombDecode binaryBits bits

golombCodesNoBlocks :: Int -> [Int] -> [Word8]
golombCodesNoBlocks !binaryBits ints = boolsToBytes (concatMap (golombCode binaryBits) ints) False

golombDecodesNoBlocks :: Int -> Integer -> [Word8] -> [Int]
golombDecodesNoBlocks !binaryBits !count word8s = decodeMany count binaryBits (concatMap (encodeBinary 8) word8s)

linewiseDiff :: [Integer] -> [Int]
linewiseDiff ints = zipWith (\ a b -> fromIntegral (a-b) ) ints (0:ints)

-- test_golomb = quickCheckWith (stdArgs {maxSuccess = 10000}) (\ ints -> let positives = map abs ints in golombDecodesNoBlocks 2 (fromIntegral $ length ints) (golombCodesNoBlocks 2 positives) == positives)