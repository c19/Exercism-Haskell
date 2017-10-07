module Beer (song) where
import Text.Printf (printf)
import Data.List (intercalate)

showBottle :: Int -> String
showBottle n
  | n > 1 = show n ++ " bottles"
  | n == 1 = "1 bottle"
  | otherwise = "No more bottles"

format :: Int -> String
format n
  | n == 0 = "No more bottles of beer on the wall, no more bottles of beer.\n\
            \Go to the store and buy some more, 99 bottles of beer on the wall.\n"
  | n == 1 = "1 bottle of beer on the wall, 1 bottle of beer.\n\
              \Take it down and pass it around, no more bottles of beer on the wall.\n"
  | otherwise = printf "%s of beer on the wall, %s of beer.\n\
                        \Take one down and pass it around, %s of beer on the wall.\n"
                        (showBottle n) (showBottle n) (showBottle $ n - 1)

song :: String
song = intercalate "\n" $ map format [99, 98..0]
