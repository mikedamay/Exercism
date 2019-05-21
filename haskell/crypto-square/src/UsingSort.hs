module UsingSort (encode2) where

import Data.List (sortBy)
import Data.Char (toLower, isAlphaNum)

encode2 :: String -> String
encode2 plainText = (encodeEx plainText)

encodeEx :: String -> [Char]
encodeEx plainText = makeChunks (sortBy colThenRow (concat $ addCoordinates squareStr numCols 0 0)) numCols numCols
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

makeChunks :: [(Int, Int, Char)] -> Int -> Int -> String
makeChunks [] _ _ = []
makeChunks (x:xs) chunkSize ctr
    | ctr == 0 = ' ' : (makeChunks (x:xs) chunkSize chunkSize)
    | otherwise = (extractText x) : (makeChunks xs chunkSize (ctr - 1))

extractText :: (Int, Int, Char) -> Char
extractText (_,_,s) = s

colThenRow :: (Int, Int, Char) -> (Int, Int, Char) -> Ordering
colThenRow (row, col, _) (orow, ocol, _) = compare (col, row) (ocol, orow)

padTo :: Int -> String -> String
padTo n str = str ++ (replicate (n - (length str)) ' ')

clean :: String -> String
clean xs = [ toLower x | x <- xs, isAlphaNum x]

