module GolombCode where
import Data.Tuple (swap)
import Data.List (unfoldr, foldl', genericLength, genericReplicate, genericSplitAt)
import Data.Word


golombCode :: Integer -> Integer -> [Bool]
golombCode modulusBits integer = encodeUnary quotient ++ encodeBinary modulusBits remainder
  where (quotient, remainder) = integer `divMod` (2 ^ modulusBits)

encodeUnary quotient = (genericReplicate quotient True) ++ [False]

encodeBinary modulusBits integer = reverse $ unfoldr numToBools (modulusBits,integer)
  where numToBools (mB,i) = if mB == 0 then Nothing else Just (odd i, (mB-1, i `div` 2))


golombDecode :: Integer -> [Bool] -> (Integer, [Bool])
golombDecode modulusBits bits = (quotient * (2 ^ modulusBits) + remainder, rest)
  where (quotient, binaryAndRest) = decodeUnary bits
        (remainder, rest) = decodeBinary modulusBits binaryAndRest

decodeUnary bits = (genericLength ones, rest)
  where (ones, z : rest) = span id bits

decodeBinary modulusBits bits = (int, rest)
  where (binaryBits, rest) = genericSplitAt modulusBits bits
        int = boolsToInt binaryBits


boolsToInt :: (Num a) => [Bool] -> a
boolsToInt = foldl' (\ z e -> z*2 + (if e then 1 else 0)) 0

boolsToBytes :: [Bool] -> Bool -> [Word8]
boolsToBytes (b7:(b6:(b5:(b4:(b3:(b2:(b1:(b0:nextBytes)))))))) padding = (boolsToInt [b7,b6,b5,b4,b3,b2,b1,b0]) : (boolsToBytes nextBytes padding)
boolsToBytes [] padding = []
boolsToBytes fewBitsShort padding = [boolsToInt $ take 8 $ fewBitsShort ++ repeat padding]


decodeMany 0 modulusBits bits = []
decodeMany n modulusBits bits = i : (decodeMany (n-1) modulusBits rest)
  where (i,rest) = golombDecode modulusBits bits


golombCodesNoBlocks :: Integer -> [Integer] -> [Word8]
golombCodesNoBlocks modulus ints = boolsToBytes (concatMap (golombCode modulus) ints) False

golombDecodesNoBlocks :: Integer -> Integer -> [Word8] -> [Integer]
golombDecodesNoBlocks modulusBits count words = decodeMany count modulusBits bits
  where bits = concatMap (encodeBinary 8) words

linewiseDiff ints = zipWith (\ a b -> a-b ) ints (0:ints)
