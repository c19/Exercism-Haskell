module RotationalCipher (rotate) where

conv :: Int -> Char -> Char
conv k c
  | 65 <= i && i <= 90 = rotate' 65 k c
  | 97 <= i && i <= 122 = rotate' 97 k c
  | otherwise = c
  where i = fromEnum c

rotate' :: Int -> Int -> Char -> Char
rotate' base k c = toEnum $ (i + k - base) `mod` 26 + base :: Char
    where base = if i < 97 then 65 else 97
          i = fromEnum c

rotate :: Int -> String -> String
rotate k = map (conv k)
