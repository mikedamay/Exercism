module Minesweeper (annotate, addCoordinates, addCoordsToLine, sweep, shred) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
-- annotate board = map mineLocationToText $ sweep board
annotate [""] = [""]
annotate board = reverse $ map convertListToString $ splitList [] areDifferentRows False $ sweep board

sweep :: [String] -> [(Int, Int, Int)]
-- sweep :: [String] -> [(Int, Int, Int)]
sweep b = (onList [] sumLists) $ map (detectMine positions) positions
    where positions = addCoordinates b


first :: (Int, Int, Int) -> Int
first (x,_,_) = x

second :: (Int, Int, Int) -> Int
second (_,y,_) = y

third :: (Int, Int, Int) -> Int
third (_,_,z) = z

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

splitList :: [[a]] -> (a -> a -> Bool) -> Bool -> [a] -> [[a]]
splitList _ _ _ [] = []
splitList [] f _ (x:y:xs) = splitList [[x]] f (f x y) (y:xs)
splitList [] f _ (x:xs) = [[x]]
splitList (z:zs) f False (x:y:xs) = splitList ((x:z):zs) f (f x y) (y:xs)
splitList (z:zs) f True (x:y:xs) = splitList ([x]:z:zs) f (f x y) (y:xs)
splitList (z:zs) f False (x:[]) = (x:z):zs
splitList (z:zs) f True (x:[]) = [x]:z:zs
-- splitList _ _ _ _
--     | otherwise = error "Didn't see that coming!"

areDifferentRows :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
areDifferentRows (a, _, _) (b, _, _) = a /= b


shred :: [Int] -> [[Int]] -> Bool -> [[Int]]
shred [] _ _ = []
shred (x:[]) (z:zs) newList = if newList then [x]:z:zs else (x:z):zs
shred (x:y:xs) [] _ = shred (y:xs) [[x]]  (isNewList x y)
    where isNewList x y = x < 30 && y > 30
shred (x:y:xs) (z:zs) newList
    | newList = shred (y:xs) ([x]:z:zs) (isNewList x y)
    | otherwise = shred (y:xs) ((x:z):zs) (isNewList x y)
    where isNewList x y = x < 30 && y > 30

onList :: [a] -> ([a] -> [a] -> [a]) -> [[a]] -> [a]
onList zs _ [] = zs
onList [] f (x:xs) = onList x f xs
onList zs f (x:xs) = onList (f x zs) f xs

sumLists :: [(Int, Int, Int)] -> [(Int, Int, Int)] -> [(Int, Int, Int)]
sumLists x y = [((first (fst p)), (second (fst p)), (third (fst p)) + (third (snd p))) | p <- (zip x y)]

mineLocationToText :: (Int, Int, Int) -> Char
mineLocationToText (_, _, val)
    | val < 0 = '*'
    | val == 0 = ' '
    | otherwise = intToDigit val

convertListToString :: [(Int, Int, Int)] -> String
convertListToString ll = reverse $ map mineLocationToText ll
