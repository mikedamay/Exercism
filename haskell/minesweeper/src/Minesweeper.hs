module Minesweeper where


import Data.Char (intToDigit)


annotate :: [String] -> [String]
annotate [] = []
annotate [""] = [""]
annotate board = process board 0

process :: [String] -> Int -> [String]
process board row
    | row == length board = []
    | otherwise = (processLine (board !! row) row 0 board) : (process board (row + 1))

processLine :: String -> Int -> Int -> [String] -> String
processLine [] _ _ _ = []
processLine (_:zs) row col board
    | col == numCols = ""
    | (board !! row) !! col == '*' = '*' : (processLine zs row (col + 1) board)
    | numAdjacentMines == 0 = ' ' : (processLine zs row (col + 1) board)
    | otherwise = (intToDigit $ (countMines neighbours board)) : (processLine zs row (col + 1) board)
    where getNeighbours = [cell | cell@(r, c) <- (adjacentCells row col), r >= 0 && r < numRows && c >= 0 && c < numCols]
          neighbours = getNeighbours
          numAdjacentMines = countMines neighbours board
          numRows = length board
          numCols = length (board !! 0)

adjacentCells :: Int -> Int -> [(Int, Int)]
adjacentCells row col =
    [
        (row - 1, col - 1), (row - 1, col), (row - 1, col + 1),
        (row, col - 1),                         (row, col + 1),
        (row + 1, col - 1), (row + 1, col), (row + 1, col + 1)
    ]

countMines :: [(Int, Int)] -> [String] -> Int
countMines neighbours board = sum (map (\(r, c) -> if ((board !! r) !! c) == '*' then 1 else 0) neighbours)

