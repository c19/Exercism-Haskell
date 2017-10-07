module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ fst $ collatz' 0 n

collatz' :: Integer -> Integer -> (Integer, Integer)
collatz' i n
  | n == 1 = (i, n)
  | even n = collatz' (i + 1) (n `div` 2)
  | otherwise = collatz' (i + 1) (n * 3 + 1)
