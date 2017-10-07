module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

type Position = (Integer, Integer)

type Robot = (Bearing, Position)

bearing :: Robot -> Bearing
bearing (b, _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (_, p) = p

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = (direction, coordinates)

simulate :: Robot -> String -> Robot
simulate robot "" = robot
simulate robot ('A':xs) = simulate (advance robot) xs
simulate robot ('L':xs) = simulate (turnleft robot) xs
simulate robot ('R':xs) = simulate (turnright robot) xs

advance :: Robot -> Robot
advance (North, (x, y)) = (North, (x, y + 1))
advance (East, (x, y)) = (East, (x + 1, y))
advance (South, (x, y)) = (South, (x, y - 1))
advance (West, (x, y)) = (West, (x - 1, y))

turnleft :: Robot -> Robot
turnleft (d, p) = (turnLeft d, p)

turnLeft :: Bearing -> Bearing
turnLeft direction = toEnum $ (3 + fromEnum direction) `mod` 4

turnright :: Robot -> Robot
turnright (d, p) = (turnRight d, p)

turnRight :: Bearing -> Bearing
turnRight direction = toEnum $ (1 + fromEnum direction) `mod` 4
