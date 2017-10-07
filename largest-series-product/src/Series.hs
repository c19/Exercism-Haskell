module Series (largestProduct) where

import Data.Char (digitToInt, isDigit)

largestProduct :: Int -> String -> Maybe Integer
largestProduct size digits
  | any (not . isDigit) digits = Nothing
  | size == 0 = Just 1
  | size < 0 = Nothing
  | size > length digits = Nothing
largestProduct size digits = Just $ toInteger $ foldl (\pre one -> if product one > pre then product one else pre) 0 $ sliding size $ map digitToInt digits

sliding :: Int -> [a] -> [[a]]
sliding n l
  | length l < n = []
  | otherwise = (take n l):(sliding n $ drop 1 l)
