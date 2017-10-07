module Diamond (diamond) where
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

diamond :: Char -> [String]
diamond ch = half ++ (drop 1 $ reverse half)
  where l = ['A'..ch]
        n = length l
        half = [getline n l c | c <- l]

getline n l c = padding ++ [c] ++ mid ++ padding
  where pre = n - (fromMaybe 0 $ elemIndex c l) - 1
        padding = replicate pre ' '
        m = 2 * n - 3 - 2 * pre
        mid = if m == 0 || c == 'A'
          then ""
          else replicate m ' ' ++ [c]
