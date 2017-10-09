module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import qualified Data.Vector as V
import qualified Data.List.Split as Split

data Matrix a = Matrix  { vect :: V.Vector a
                        , rows :: Int
                        , cols :: Int
                        } deriving (Eq, Show)

-- cols :: Matrix a -> Int
-- cols matrix = error "You need to implement this function."

column :: Int -> Matrix a -> V.Vector a
column x (Matrix vect rows cols) = V.map ((V.!) vect) (V.enumFromStepN x cols rows)

flatten :: Matrix a -> V.Vector a
flatten (Matrix vect _ _) = vect

fromList :: [[a]] -> Matrix a
fromList xss = Matrix (V.concat $ map V.fromList xss) (length xss) (length $ head xss)

fromString :: Read a => String -> Matrix a
fromString xs = Matrix (V.fromList list) rows cols
  where list :: Read a => [a]
        list = map read $ split xs
        ls = map split $ lines xs
        rows = length ls
        cols = if rows /= 0
               then length $ head ls
               else 0
        split = Split.split (Split.dropBlanks . Split.dropDelims $ Split.oneOf " \n")


reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (new_rows, new_cols) (Matrix vect rows cols)
  | new_rows * new_cols /= rows * cols = error "rows*cols doesn't match"
  | otherwise = Matrix vect new_rows new_cols

row :: Int -> Matrix a -> V.Vector a
row x (Matrix vect rows cols) = V.slice (x * cols) cols vect

shape :: Matrix a -> (Int, Int)
shape (Matrix vect rows cols) = (rows, cols)

transpose :: Matrix a -> Matrix a
transpose matrix@(Matrix vect rows cols) = (Matrix new_vect new_rows new_cols)
  where new_vect = V.concatMap gen_row $ V.enumFromStepN 0 1 new_rows
        new_rows = cols
        new_cols = rows
        gen_row  = \new_row -> column new_row matrix
