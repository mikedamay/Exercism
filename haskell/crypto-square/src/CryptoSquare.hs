module CryptoSquare (encode) where

import Data.Char (toLower, isAlphaNum)

encode :: String -> String
encode plainText = makeChunks (colsToRows squareStr numCols numRows) numCols numCols
    where
        squareStr = padTo squareSize (clean plainText)
        squareSize = numCols * numRows
        (numCols, numRows) = dims (clean plainText)

dims :: String -> (Int, Int)
dims s = ((ceiling len), (ceiling len))
    where len = sqrt (fromIntegral (length s))

makeChunks :: [Char] -> Int -> Int -> String
makeChunks [] _ _ = []
makeChunks (x:xs) chunkSize ctr
    | ctr == 0 = ' ' : (makeChunks (x:xs) chunkSize chunkSize)
    | otherwise = x : (makeChunks xs chunkSize (ctr - 1))

padTo :: Int -> String -> String
padTo n str = str ++ (replicate (n - (length str)) ' ')

clean :: String -> String
clean xs = [ toLower x | x <- xs, isAlphaNum x]

colsToRows :: String -> Int -> Int -> String
colsToRows xs numCols rows = concat $ map gather [0..(rows - 1)]
    where gather = gatherRow xs numCols 0

gatherRow :: String -> Int -> Int -> Int -> String
gatherRow [] _ _ _ = []
gatherRow (x:xs) numCols ctr row
    | (ctr - row) `mod` numCols == 0 = [x] ++ (gatherRow xs numCols (ctr + 1) row)
    | otherwise = gatherRow xs numCols (ctr + 1) row