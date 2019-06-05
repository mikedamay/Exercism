module Minesweeper (annotate) where

import Data.Char (intToDigit)

type Row = Int
type Col = Int
type Val = Int


annotate :: [String] -> [String]
annotate [""] = [""]
annotate board = cellsToText $ sweep $ textToCells board
    where cellsToText = cellListsToText . boardToMultipleLists
          cellListsToText = reverse . (map convertListToString)
          boardToMultipleLists = splitList [] areDifferentRows False

sweep :: [(Row, Col, Val)] -> [(Row, Col, Val)]
sweep cells = (onList [] sumSubLists) $ map (detectMine cells) cells

textToCells :: [String] -> [(Row, Col, Val)]
textToCells [] = []
textToCells b = concat $ map lineToCells $ zip b [0 :: Int ..]

lineToCells :: ([Char], Int) -> [(Row, Col, Val)]
lineToCells (line,row) = map (\x -> (row, snd x, if (fst x) == '*' then -1 else 0) ) $ zip line [0..]

-- detectMine examines each cell together with every cell on the board to find out if they are neighbours
-- candidateNeighbours - every cell on the board.
detectMine :: [(Row, Col, Val)] -> (Row, Col, Val) -> [(Row, Col, Val)]
detectMine candidateNeighbours cell = map (addAdjacent cell) candidateNeighbours

addAdjacent :: (Row, Col, Val) -> (Row, Col, Val) -> (Row, Col, Val)
addAdjacent cell candidate@(row, col, val) = if val == -1 then (row, col, -1) else if isNeighbour cell candidate then (row, col, 1) else (row, col, 0)

isNeighbour :: (Row, Col, Val) -> (Row, Col, Val) -> Bool
isNeighbour (row, col, val) (candRow, candCol, candVal) =
    val == -1 && (candRow /= row || candCol /= col) && candVal /= -1 && abs(candRow - row) <= 1 && abs(candCol - col) <= 1

splitList :: [[a]] -> (a -> a -> Bool) -> Bool -> [a] -> [[a]]
splitList _ _ _ [] = []
splitList [] f _ (x:y:xs) = splitList [[x]] f (f x y) (y:xs)
splitList [] _ _ (x:[]) = [[x]]
splitList (z:zs) f False (x:y:xs) = splitList ((x:z):zs) f (f x y) (y:xs)
splitList (z:zs) f True (x:y:xs) = splitList ([x]:z:zs) f (f x y) (y:xs)
splitList (z:zs) _ False (x:[]) = (x:z):zs
splitList (z:zs) _ True (x:[]) = [x]:z:zs

areDifferentRows :: (Row, Col, Val) -> (Row, Col, Val) -> Bool
areDifferentRows (a, _, _) (b, _, _) = a /= b

onList :: [a] -> ([a] -> [a] -> [a]) -> [[a]] -> [a]
onList zs _ [] = zs
onList [] f (x:xs) = onList x f xs
onList zs f (x:xs) = onList (f x zs) f xs

sumSubLists :: [(Row, Col, Val)] -> [(Row, Col, Val)] -> [(Row, Col, Val)]
sumSubLists = zipWith mergeCellInstanes
-- sumSubLists x y = [((first (fst p)), (second (fst p)), (third (fst p)) + (third (snd p))) | p <- (zip x y)]

mergeCellInstanes :: (Row, Col, Val) -> (Row, Col, Val) -> (Row, Col, Val)
mergeCellInstanes (xrow, xcol, xval) (_, _, yval) = (xrow, xcol, xval + yval)


cellToChar :: (Row, Col, Val) -> Char
cellToChar (_, _, val)
    | val < 0 = '*'
    | val == 0 = ' '
    | otherwise = intToDigit val

convertListToString :: [(Row, Col, Val)] -> String
convertListToString = reverse . map cellToChar

