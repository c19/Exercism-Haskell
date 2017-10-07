module Roman (numerals) where

import Control.Monad (liftM2, liftM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Debug.Trace

base = [(1, Just "I"),
        (5, Just "V"),
        (10, Just "X"),
        (50, Just "L"),
        (100, Just "C"),
        (500, Just "D"),
        (1000, Just "M")]
baseMap = Map.fromList base

stickM = liftM2 (++)

minusType n = or [Map.member (n + i) baseMap | i <- map fst base]

numerals :: Integer -> Maybe String

numerals n
  | n < 1 || n > 5000 = Nothing
  | Map.member n baseMap = Map.findWithDefault Nothing n baseMap
  | minusType n = letter `stickM` Map.findWithDefault Nothing (n + i) baseMap
  | multi > 0 && remainder == 0 = concat <$> (replicate (fromInteger multi) <$> numerals pivot)
  | multi == 1 && pivot < m = (++) <$> numerals m <*> (Just $ fromMaybe "" $ numerals (n - m))
  | otherwise =(++) <$> numerals (n - remainder) <*> (Just $ fromMaybe "" $ numerals remainder)
    where pivot = head $ dropWhile (n <) $ reverse $ map fst base
          multi = n `div` pivot
          remainder = n - multi * pivot
          (i, letter) = head $ filter (\(i, _) -> Map.member (n + i) baseMap) base
          m = head [i - j | i <- reverse $ map fst base, j <- map fst base, n > i - j, i - j > 0]
