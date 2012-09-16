{-# LANGUAGE BangPatterns #-}
module GolombCode where
import Data.Word
import Test.QuickCheck
import qualified Data.Sequence as S
import qualified Data.Foldable as F

sconcat = F.foldl' (S.><) S.empty

golombCode :: Integer -> Integer -> S.Seq Bool
golombCode modulusBits integer = encodeUnary quotient S.>< encodeBinary modulusBits remainder
  where (quotient, remainder) = integer `divMod` (2 ^ modulusBits)

encodeUnary quotient = (S.replicate (fromIntegral quotient) True) S.|> False

encodeBinary modulusBits integer = S.reverse (S.unfoldr numToBools (modulusBits,integer))
  where numToBools (mB,i) = if mB == 0 then Nothing else Just (odd i, (mB-1, i `div` 2))

golombDecode :: Integer -> S.Seq Bool -> (Integer, S.Seq Bool)
golombDecode modulusBits bits = (quotient * (2 ^ modulusBits) + remainder, rest)
  where (quotient, binaryAndRest) = decodeUnary bits
        (remainder, rest) = decodeBinary modulusBits binaryAndRest

decodeUnary bits = (fromIntegral (S.length ones), rest)
  where (ones, zConsRest) = S.spanl id bits
        rest = S.drop 1 zConsRest

decodeBinary modulusBits bits = (int, rest)
  where (binaryBits, rest) = S.splitAt (fromIntegral modulusBits) bits
        int = boolsToInt binaryBits

boolsToInt :: (Num a) => S.Seq Bool -> a
boolsToInt = F.foldl' (\ z e -> z*2 + (if e then 1 else 0)) 0

boolsToBytes :: S.Seq Bool -> Bool -> S.Seq Word8
boolsToBytes sbool padding = sword
  where (sword, _, _) = F.foldl' f (S.empty, 0, 0) paddedSBool
        f (!bytes, !accum, !bitCount) bit = let newAccum=2*accum + if bit then 1 else 0 in if bitCount==7 then (bytes S.|> newAccum,0,0) else (bytes, newAccum, bitCount+1)
	paddedSBool = sbool S.>< (S.replicate 7 padding)

decodeMany 0 modulusBits bits = S.empty
decodeMany n modulusBits bits = i S.<| (decodeMany (n-1) modulusBits rest)
  where (i,rest) = golombDecode modulusBits bits

golombCodesNoBlocks :: Integer -> [Integer] -> [Word8]
golombCodesNoBlocks modulus ints = F.toList (boolsToBytes (sconcat (map (golombCode modulus) ints)) False)

golombDecodesNoBlocks :: Integer -> Integer -> [Word8] -> [Integer]
golombDecodesNoBlocks modulusBits count bytes = F.toList (decodeMany count modulusBits bits)
  where bits = sconcat (map (encodeBinary 8) bytes)

test_golomb = quickCheckWith (stdArgs {maxSuccess = 10000}) (\ ints -> let positives = map abs ints in golombDecodesNoBlocks 2 (fromIntegral $ length ints) (golombCodesNoBlocks 2 positives) == positives)

linewiseDiff ints = zipWith (\ a b -> a-b ) ints (0:ints)
