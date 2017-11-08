module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = [row (toInteger i) | i <- [0..(x-1)]]

row :: Integer -> [Integer]
row 0 = [1]
row n = [combine n i | i <- [0..n]]

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = product [1..n]

combine :: Integer -> Integer -> Integer
combine n 0 = 1
combine n k = ((factorial n) `div` (factorial k)) `div` (factorial (n - k))
