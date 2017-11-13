module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c) = a * a + b * b + c * c == 2 * m * m
  where m = maximum [a, b, c]

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = filter isPythagorean [(a, b, c) | a <- [minFactor..maxFactor], b <- [a..maxFactor], c <- [b..maxFactor]]
