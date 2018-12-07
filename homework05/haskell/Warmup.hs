module Warmup
  (change
  , stripQuotes)
  where


change :: Int
change amount =
    if amount < 0 then
        Left "Amount can be negative"
    else
        let
            (quarter, afterQuarters) = amount 'divMod' 25
            (dimes, afterDimes) = afterQuarters 'divMod' 10
            (nickels, pennies) = afterDimes 'divMod' 5
          in
        Right (quarters, dimes, nickels, pennies)




powers :: Double
powers value =
