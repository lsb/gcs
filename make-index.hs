import System.Environment (getArgs)

main = do [unaryBits, indexFrequency] <- getArgs
          interact (show . every (read indexFrequency) . sumsBitcounts (read unaryBits) . map read . lines)

sumsBitcounts :: Integer -> [Integer] -> [[Integer]]
sumsBitcounts unaryBits = map pairToSum . linewisePairsSum . map (\ di -> (di, sizeof unaryBits di) ) . linewiseDiff

pairToSum (a,b) = [a,b]

linewisePairsSum = scanl1 (\ (xTot,yTot) (x,y) -> (xTot+x, yTot+y) )

linewiseDiff is = zipWith (-) is (0:is)

sizeof unaryBits i = (i `div` (2 ^ unaryBits)) + 1 + unaryBits

every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []