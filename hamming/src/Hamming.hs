module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = Just $ distance' xs ys
  | otherwise = Nothing

distance' :: String -> String -> Int
distance' xs ys = sum $ zipWith (\a b -> if a == b then 0 else 1) xs ys
