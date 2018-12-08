module Warmup
  (change
  , stripQuotes
  -- , sumOfCubesOfOdds
  , swapAdjacents)
  where


change :: Int -> Either String (Int, Int, Int, Int)
change amount =
    if amount < 0 then
        Left "amount cannot be negative"
    else
        let
            (quarters, afterQuarters) = amount `divMod` 25
            (dimes, afterDimes) = afterQuarters `divMod` 10
            (nickels, pennies) = afterDimes `divMod` 5
          in
        Right (quarters, dimes, nickels, pennies)

stripQuotes s = filter (\c -> c /= '\'' && c /= '"') s

-- sumOfCubesOfOdds :: Int -> Int -> [a] -> [a]
-- sumOfCubesOfOdds (h:t) : if (not . even) h then h*h*h + (sumOfCubesOfOdds t) else sumOfCubesOfOdds t


swapAdjacents [] = []
swapAdjacents xs@[_] = xs
swapAdjacents (a : b : xs) = b : a : swapAdjacents xs
