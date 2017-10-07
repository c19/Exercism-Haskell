module RunLength (decode, encode) where
import Data.Char (isDigit)

readNum :: String -> Int
readNum = read . takeWhile isDigit

dropNum :: String -> String
dropNum = dropWhile isDigit

decode :: String -> String
decode [] = []
decode xs = expanded ++ decode rs
  where
    rs' = dropNum xs
    c = head rs'
    rs = tail rs'
    expanded = if isDigit $ head xs
      then replicate (readNum xs) c
      else [c]

count :: Eq a => a -> [a] -> Int
count x = length . takeWhile (== x)

encode :: String -> String
encode [] = []
encode [x] = [x]
encode (x:xs) = sn ++ [x] ++ encode (drop n xs)
  where
    n = count x xs
    sn = if n > 0
      then show $ n + 1 -- x already take one
      else []
