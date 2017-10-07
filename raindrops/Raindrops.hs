module Raindrops (convert) where

m :: [(Int, String)]
m = [
  (3, "Pling"),
  (5, "Plang"),
  (7, "Plong")]

convert :: Int -> String
convert n = w
  where
    s = concatMap (\(a, b) -> if n `mod` a == 0 then b else "") m
    w = if s == "" then show n else s
