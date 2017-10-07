module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f z [] = z
foldl' f z (x:xs) = foldl' f (z `seq` f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x z'
  where z' = foldr f z xs

length :: [a] -> Int
length xs = foldl' (\a b -> a + 1) 0 xs

reverse :: [a] -> [a]
reverse = foldl' (\a b -> b:a) []

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p xs = foldr (\a acc -> if p a then a:acc else acc) [] xs

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat [] = []
concat xss = foldr (++) [] xss
