import System.Environment (getArgs)

main = do [unaryBits, indexFrequency] <- getArgs
          interact (show . every (read indexFrequency) . sumsBitcounts (read unaryBits) . map read . lines)

sumsBitcounts :: Integer -> [Integer] -> [[Integer]]
sumsBitcounts unaryBits = linewiseTupleSum . map (\ di -> [di, sizeof unaryBits di, 1] ) . linewiseDiff

linewiseTupleSum = scanl1 (zipWith (+))

linewiseDiff is = zipWith (-) is (0:is)

sizeof unaryBits i = (i `div` (2 ^ unaryBits)) + 1 + unaryBits

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []