module IsbnVerifier (isbn) where

import Data.Char (isDigit, digitToInt)

isbn :: String -> Bool
isbn s = length (filter (/= '-') s) == 10 && length nums == 9 && verify > -1 && 0 == (verify + sum (zipWith (*) [10,9..] nums)) `mod` 11
  where nums = map digitToInt $ filter isDigit $ init s
        verify = check $ last s

check :: Char -> Int
check 'x' = 10
check 'X' = 10
check n | isDigit n = digitToInt n
check _ = -1
