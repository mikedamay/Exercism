module CryptoSquare (encode, dims, strip) where

import Data.List (sortBy)

encode :: String -> String
encode xs = show (length xs) 

dims :: String -> (Int, Int)
dims s = ((ceiling len), (floor len))
    where len = sqrt (fromIntegral (length s))

stripper :: String -> [(Int, Int, Char)]
stripper xs = sortBy getKey (concat $ strip xs numCols 0 0)
    where (numCols, numRows) = dims xs

strip :: String -> Int -> Int -> Int -> [[(Int, Int, Char)]]
strip [] _ _ _ = []
strip (x:xs) cols col row
    | col == cols = strip (x:xs) cols 0 (row + 1)
    | col == 0 = [(grab (x:xs) cols 0 row)] ++ (strip xs cols (col + 1) row)
    | otherwise = strip xs cols (col + 1) row

grab :: String -> Int -> Int -> Int -> [(Int, Int, Char)]
grab [] _ _ _ = []
grab (x:xs) numCols col row
    | col == numCols = []
    | otherwise = [(row, col, x)] ++ (grab xs numCols (col + 1) row)

getKey :: (Int, Int, Char) -> (Int, Int, Char) -> Ordering
getKey (row, col, ch) (orow, ocol, och) = compare (col, row) (ocol, orow)

{-
tt :: String -> [String]
tt [] = []
tt (x:xs) = [[x]] ++ [(grab xs 2 2)]
-}

-- square root of length
-- round up for cols, round down for rows
-- pad with spaces
-- build list of strings
-- 
