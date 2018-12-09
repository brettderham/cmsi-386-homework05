module Warmup
  (change
  , stripQuotes
  , firstUppercasedOverLengthFive
  , powers
  , sumOfCubesOfOdds
  , swapAdjacents
  , Shape (Box, Sphere)
  , volume
  , surfaceArea)
  where
import Data.Char (toLower, toUpper)
import Data.List

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

firstUppercasedOverLengthFive :: [String] -> Maybe String
firstUppercasedOverLengthFive [] = Nothing
firstUppercasedOverLengthFive (x:xs)
   | (length x) > 5 = Just (map toUpper x)
   | otherwise = firstUppercasedOverLengthFive xs

powers :: Num a => a -> [a]
powers n = (n^0) : map (* n) (powers n)

sumOfCubesOfOdds :: Integral n => [n] -> n
sumOfCubesOfOdds [] = 0
sumOfCubesOfOdds (x:xs)
    | odd x = x^3 + sumOfCubesOfOdds xs
    | otherwise = sumOfCubesOfOdds xs

swapAdjacents [] = []
swapAdjacents xs@[_] = xs
swapAdjacents (a : b : xs) = b : a : swapAdjacents xs

data Shape
    = Sphere Double
    | Box Double Double Double
    deriving (Eq, Show)

volume :: Shape -> Double
volume (Sphere r) = ( 4 / 3 ) * pi * r * r * r
volume (Box h l w) = h * l * w

surfaceArea :: Shape -> Double
surfaceArea (Sphere r) = 4 * pi * r * r
surfaceArea (Box h l w) = (2 * h * w) + (2 * h * l) + (2 * l *w)
