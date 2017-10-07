module Triangle (TriangleType(..), triangleType) where
import Data.List (sort)
import qualified Data.Set as Set

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType l m n
  | any (<=0) [l, m, n] = Illegal
  | x + y <= z = Illegal
  | setLen == 1 = Equilateral
  | setLen == 2 = Isosceles
  | otherwise = Scalene
    where
      [x, y, z] = sort [l, m, n]
      setLen = length $ Set.fromList [l, m, n]
