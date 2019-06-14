module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum, Bounded, Ord)

right :: Robot -> Robot
right (r, c, b) | b == West = (r, c, North)
                | otherwise = (r, c, succ b)

left :: Robot -> Robot
left (r, c, b) | b == North = (r, c, West)
               | otherwise = (r, c, pred b)

advance :: Robot -> Robot
advance (r, c, North) = (r, c + 1, North)
advance (r, c, South) = (r, c - 1, South)
advance (r, c, East) = (r + 1, c, East)
advance (r, c, West) = (r - 1, c, West)

moveit :: Robot -> Char -> Robot
moveit r 'L' = left r
moveit r 'R' = right r
moveit r 'A' = advance r
moveit _ _ = error "stuff happens"

simulate :: Robot -> [Char] -> Robot
simulate r = foldl moveit r

type Robot = (Integer, Integer, Bearing)

bearing :: Robot -> Bearing
bearing (_, _, b) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (r, c, _) = (r, c)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (r, c) = (r, c, direction)

move :: Robot -> String -> Robot
move robot instructions = simulate robot instructions
