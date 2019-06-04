module Minesweeper (annotate, addCoordinates, addCoordsToLine, sweep) where

annotate :: [String] -> [String]
annotate board = []

sweep :: [String] -> [Int]
-- sweep :: [String] -> [(Int, Int, Int)]
sweep b = sum $ map (detectMine positions) positions
    where positions = addCoordinates b


thrd :: (Int, Int, Int) -> Int
thrd (x,y,z) = z

addCoordinates :: [String] -> [(Int, Int, Int)]
addCoordinates [] = []
addCoordinates b = concat $ map addCoordsToLine $ zip b [0 :: Int ..]

addCoordsToLine :: ([Char], Int) -> [(Int, Int, Int)]
addCoordsToLine (line,row) = map (\x -> (row, snd x, if (fst x) == '*' then -1 else 0) ) $ zip line [0..]

detectMine :: [(Int, Int, Int)] -> (Int, Int, Int) -> [Int]
detectMine positions cell = map thrd $ map (addAdjacent cell) positions

addAdjacent :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
addAdjacent cell candidate@(row, col, val) = if val == -1 then (row, col, -1) else if isNeighbour cell candidate then (row, col, 1) else (row, col, 0)

isNeighbour :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
isNeighbour cell@(row, col, val) candidate@(candRow, candCol, candVal) =
    val == -1 && (candRow /= row || candCol /= col) && candVal /= -1 && abs(candRow - row) <= 1 && abs(candCol - col) <= 1