import System.Environment (getArgs)

main = do [binaryBits, indexFrequency] <- getArgs
          interact (show . every (read indexFrequency) . sumsBitcounts (read binaryBits) . map read . lines)

sumsBitcounts :: Integer -> [Integer] -> [[Integer]]
sumsBitcounts binaryBits = linewiseTupleSum . map (\ di -> [di, sizeof binaryBits di, 1] ) . linewiseDiff

linewiseTupleSum = scanl1 (zipWith (+))

linewiseDiff is = zipWith (-) is (0:is)

sizeof binaryBits i = (i `div` (2 ^ binaryBits)) + 1 + binaryBits

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []