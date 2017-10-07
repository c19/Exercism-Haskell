module Isogram (isIsogram) where
import Data.Char (toLower, isLetter)
import qualified Data.Set as Set

isIsogram :: String -> Bool
isIsogram s = length d == length (Set.fromList $ map toLower d)
  where d = filter isLetter s
