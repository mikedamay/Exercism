module Minesweeper (annotate, addCoordinates, addCoordsToLine, sweep, shred) where

annotate :: [String] -> [String]
annotate board = []

sweep :: [String] -> [[(Int, Int, Int)]]
-- sweep :: [String] -> [(Int, Int, Int)]
sweep b = map (detectMine positions) positions
    where positions = addCoordinates b


thrd :: (Int, Int, Int) -> Int
thrd (x,y,z) = z

addCoordinates :: [String] -> [(Int, Int, Int)]
addCoordinates [] = []
addCoordinates b = concat $ map addCoordsToLine $ zip b [0 :: Int ..]

addCoordsToLine :: ([Char], Int) -> [(Int, Int, Int)]
addCoordsToLine (line,row) = map (\x -> (row, snd x, if (fst x) == '*' then -1 else 0) ) $ zip line [0..]

detectMine :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
detectMine positions cell = map (addAdjacent cell) positions

addAdjacent :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addAdjacent cell candidate@(row, col, val) = if val == -1 then (row, col, -1) else if isNeighbour cell candidate then (row, col, 1) else (row, col, 0)

isNeighbour :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isNeighbour cell@(row, col, val) candidate@(candRow, candCol, candVal) =
    val == -1 && (candRow /= row || candCol /= col) && candVal /= -1 && abs(candRow - row) <= 1 && abs(candCol - col) <= 1

-- [] [1,2,3,40,50,60]
-- [[1]] [2,3,40,50,60]
-- [[1,2]] [3,40,50,60]
-- [[1,2,3]] [40,50,60]
-- [[1,2,3],[40]] [50,60]
-- [[1,2,3],[40,50]] [60]
-- [[1,2,3],[40,50,60]] []



shred :: [Int] -> [[Int]] -> Bool -> [[Int]]
shred [] _ _ = []
shred (x:[]) (z:zs) newList = if newList then [x]:z:zs else (x:z):zs
shred (x:y:xs) [] _ = shred (y:xs) [[x]]  (isNewList x y)
    where isNewList x y = x < 30 && y > 30
shred (x:y:xs) (z:zs) newList
    | newList = shred (y:xs) ([x]:z:zs) (isNewList x y)
    | otherwise = shred (y:xs) ((x:z):zs) (isNewList x y)
    where isNewList x y = x < 30 && y > 30

onList :: [Int] -> [[Int]] -> ([Int] -> [Int] -> [Int]) -> [Int]
onList zs [] _ = zs
onList [] (x:xs) f = onList x xs f
onList zs (x:xs) f = onList (f x zs) xs f

sumLists :: [Int] -> [Int] -> [Int]
sumLists x y = [(fst p) + (snd p) | p <- (zip x y)]
