{-# LANGUAGE TypeSynonymInstances #-}

module Clock (clockHour, clockMin, fromHourMin, toString) where
import Text.Printf (printf)

newtype Clock = Clock Int deriving (Show, Eq)

instance Num Clock where
  abs = id
  signum _ = Clock 1
  (*) (Clock a) (Clock b) = fromMin $ a * b
  (+) (Clock a) (Clock b) = fromMin $ a + b
  negate (Clock clock) = fromMin $ 24 * 60 - clock
  fromInteger i = fromMin (fromInteger i)

clockHour :: Clock -> Int
clockHour (Clock clock) = (clock `div` 60) `mod` 24

clockMin :: Clock -> Int
clockMin (Clock clock) = clock `mod` 60

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = fromMin (hour * 60 + min)

fromMin :: Int -> Clock
fromMin min
  | min < 0 = fromMin $ min + 24 * 60
  | otherwise = Clock (min `mod` (24 * 60))

toString :: Clock -> String
toString clock = Text.Printf.printf "%02d:%02d" (clockHour clock) (clockMin clock)
