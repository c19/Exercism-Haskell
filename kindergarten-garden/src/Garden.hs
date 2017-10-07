module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

import qualified Data.Map.Strict as M
import Data.List (sort)

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)

parse :: Char -> Plant
parse 'C' = Clover
parse 'G' = Grass
parse 'R' = Radishes
parse 'V' = Violets
parse _ = error "unkown plant"

kids :: [String]
kids = ["Alice", "Bob", "Charlie", "David",
        "Eve", "Fred", "Ginny", "Harriet",
        "Ileana", "Joseph", "Kincaid", "Larry"]

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

defaultGarden :: String -> M.Map String [Plant]
defaultGarden = garden kids

garden :: [String] -> String -> M.Map String [Plant]
garden students plants = M.fromList $ zipWith3 (\p1 p2 k -> (k, map parse p1 ++ map parse p2)) l1' l2' $ sort students
  where [l1, l2] = lines plants
        [l1', l2'] = [group 2 l1, group 2 l2]

lookupPlants :: String -> M.Map String [Plant] -> [Plant]
lookupPlants student garden = M.findWithDefault [] student garden
