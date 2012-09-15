module GolombCode (golombCode, golombDecode, encodeUnary, decodeUnary, encodeBinary, decodeBinary) where
import Data.Tuple (swap)
import Data.List (unfoldr, foldl', genericLength, genericReplicate, genericSplitAt)


golombCode :: Integer -> Integer -> [Bool]
golombCode modulusBits integer = encodeUnary quotient ++ encodeBinary modulusBits remainder
  where (quotient, remainder) = integer `divMod` (2 ^ modulusBits)

encodeUnary quotient = (genericReplicate quotient True) ++ [False]

encodeBinary modulusBits integer = reverse $ unfoldr (\ (mB,i) -> if mB == 0 then Nothing else Just (odd i, (mB-1, i `div` 2))) (modulusBits,integer)


golombDecode :: Integer -> [Bool] -> (Integer, [Bool])
golombDecode modulusBits bits = (quotient * (2 ^ modulusBits) + remainder, rest)
  where (quotient, binaryAndRest) = decodeUnary bits
        (remainder, rest) = decodeBinary modulusBits binaryAndRest

decodeUnary bits = (genericLength ones, rest)
  where (ones, z : rest) = span id bits

decodeBinary modulusBits bits = (int, rest)
  where (binaryBits, rest) = genericSplitAt modulusBits bits
        int = foldl' (\ z e -> z * 2 + if e then 1 else 0) 0 binaryBits
