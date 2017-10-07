module Allergies (Allergen(..), allergies, isAllergicTo) where

import qualified Data.Map.Strict as M
import Data.Bits ((.&.))

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq, Ord, Show)

allergenList :: [(Allergen, Int)]
allergenList =[(Eggs, 1)
              ,(Peanuts, 2)
              ,(Shellfish, 4)
              ,(Strawberries, 8)
              ,(Tomatoes, 16)
              ,(Chocolate, 32)
              ,(Pollen, 64)
              ,(Cats, 128)
              ]

allergenMap :: M.Map Allergen Int
allergenMap = M.fromList allergenList

allergenMap' :: M.Map Int Allergen
allergenMap' = M.fromList $ map (\(a, b) -> (b, a)) allergenList

instance Enum Allergen where
  fromEnum e = M.findWithDefault 1 e allergenMap
  toEnum i = M.findWithDefault Eggs i allergenMap'

allergies :: Int -> [Allergen]
allergies score = [a | a <- M.keys allergenMap, isAllergicTo a score]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo allergen score = fromEnum allergen .&. score > 0
