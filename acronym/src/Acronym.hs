module Acronym (abbreviate) where
import Data.Char (isLetter, isUpper, isLower, toUpper)

tokenize :: String -> [String]
tokenize [] = []
tokenize xs = token : tokenize rs
  where
    xs' = dropWhile (not . isLetter) xs
    token = takeWhile isLetter xs'
    rs = drop (length token) xs'

toCapital :: String -> String
toCapital token
  | all isUpper token = [toUpper $ head token]
  | all isLower token = [toUpper $ head token]
  | otherwise = map toUpper $ filter isUpper token

abbreviate :: String -> String
abbreviate xs = concatMap toCapital $ tokenize xs
