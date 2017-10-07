module ETL (transform) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = M.fromList $ concatMap (\(n, xs) -> zip (map toLower xs) (repeat n)) $ M.toList legacyData
