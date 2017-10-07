module Pangram (isPangram) where

import Data.Char (toLower)
import qualified Data.Set as Set

alphabet :: Set.Set Char
alphabet = Set.fromList "abcdefghijklmnopqrstuvwxyz"

isPangram :: String -> Bool
isPangram text = Set.isSubsetOf alphabet $ Set.fromList $ map toLower  text
