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

data Robot = Robot (Integer, Integer) Bearing deriving (Show)


right :: Robot -> Robot
right (Robot (col, row) b)
    | b == West = Robot (col, row) North
    | otherwise = Robot (col, row) (succ b)

left :: Robot -> Robot
left (Robot (col, row) b)
    | b == North = Robot (col, row) West
    | otherwise = Robot (col, row) (pred b)

advance :: Robot -> Robot
advance (Robot (col, row) b)
    | b == North = Robot (col, row + 1) b
    | b == South = Robot (col, row - 1) b
    | b == East = Robot (col + 1, row) b
    | b == West = Robot (col - 1, row) b
advance (Robot (_, _) _) = error "This, I do not understand - needed to avoid missing match warning"

moveit :: Robot -> Char -> Robot
moveit r 'L' = left r
moveit r 'R' = right r
moveit r 'A' = advance r
moveit _ _ = error "stuff happens"

simulate :: Robot -> [Char] -> Robot
simulate r = foldl moveit r

bearing :: Robot -> Bearing
bearing (Robot _ b) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot (col, row) _) = (col, row)

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction (col, row) = Robot (col, row) direction

move :: Robot -> String -> Robot
move robot instructions = simulate robot instructions
