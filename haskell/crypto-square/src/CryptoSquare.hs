module CryptoSquare (encode, dims, clean, padTo, getKey, fmt) where

import Data.List (sortBy)
import Data.Char (toLower, isLetter, isAlphaNum)

encode :: String -> String
encode plainText = (encodeEx plainText)

encodeEx :: String -> [Char]
encodeEx plainText = fmt (sortBy getKey (concat $ addCoordinates squareStr numCols 0 0)) numCols numCols
    where
        squareStr = padTo squareSize (clean plainText)
        squareSize = numCols * numRows
        (numCols, numRows) = dims (clean plainText)

dims :: String -> (Int, Int)
dims s = ((ceiling len), (ceiling len))
    where len = sqrt (fromIntegral (length s))

addCoordinates :: String -> Int -> Int -> Int -> [[(Int, Int, Char)]]
addCoordinates [] _ _ _ = []
addCoordinates (x:xs) cols col row
    | col == cols = addCoordinates (x:xs) cols 0 (row + 1)
    | col == 0 = [(grab (x:xs) cols 0 row)] ++ (addCoordinates xs cols (col + 1) row)
    | otherwise = addCoordinates xs cols (col + 1) row

grab :: String -> Int -> Int -> Int -> [(Int, Int, Char)]
grab [] _ _ _ = []
grab (x:xs) numCols col row
    | col == numCols = []
    | otherwise = [(row, col, x)] ++ (grab xs numCols (col + 1) row)

fmt :: [(Int, Int, Char)] -> Int -> Int -> String
fmt [] _ _ = []
fmt (x:xs) chunkSize ctr
    | ctr == 0 = ' ' : (fmt (x:xs) chunkSize chunkSize)
    | otherwise = (extract x) : (fmt xs chunkSize (ctr - 1))

extract :: (Int, Int, Char) -> Char
extract (r,c,s) = s

getKey :: (Int, Int, Char) -> (Int, Int, Char) -> Ordering
getKey (row, col, ch) (orow, ocol, och) = compare (col, row) (ocol, orow)

padTo :: Int -> String -> String
padTo n str = str ++ (replicate (n - (length str)) ' ')

clean :: String -> String
clean xs = [ toLower x | x <- xs, isAlphaNum x]

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
