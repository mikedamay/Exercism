module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate [""] = [""]
annotate board = cellListsToText $ boardToMultipleLists $ sweep board

sweep :: [String] -> [(Int, Int, Int)]
sweep b = (onList [] sumSubLists) $ map (detectMine positions) positions
    where positions = addCoordinates b

cellListsToText :: [[(Int, Int, Int)]] -> [String]
cellListsToText = reverse . (map convertListToString)

boardToMultipleLists :: [(Int, Int, Int)] -> [[(Int, Int, Int)]]
boardToMultipleLists = splitList [] areDifferentRows False

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

areDifferentRows :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
areDifferentRows (a, _, _) (b, _, _) = a /= b

onList :: [a] -> ([a] -> [a] -> [a]) -> [[a]] -> [a]
onList zs _ [] = zs
onList [] f (x:xs) = onList x f xs
onList zs f (x:xs) = onList (f x zs) f xs

sumSubLists :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
sumSubLists = zipWith mergeCellInstanes
-- sumSubLists x y = [((first (fst p)), (second (fst p)), (third (fst p)) + (third (snd p))) | p <- (zip x y)]

mergeCellInstanes :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
mergeCellInstanes (xrow, xcol, xval) (_, _, yval) = (xrow, xcol, xval + yval)


mineLocationToText :: (Int, Int, Int) -> Char
mineLocationToText (_, _, val)
    | val < 0 = '*'
    | val == 0 = ' '
    | otherwise = intToDigit val

convertListToString :: [(Int, Int, Int)] -> String
convertListToString = reverse . map mineLocationToText

-- locateMines :: [(Int, Int, Int)] -> [[(Int,Int,Int)]]
-- locateMines cells = [(first x, second x, valForXY x y) | x <- cells, y <- cells]
--
-- isNeighbourOf :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
-- isNeighbourOf = isNeighbour
--
-- valForXY :: (Int, Int, Int) -> (Int, Int, Int) -> Int
-- valForXY x y
--     | x `isNeighbourOf` y = 1
--     | otherwise = 0
