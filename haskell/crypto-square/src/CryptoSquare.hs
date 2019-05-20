module CryptoSquare (encode, dims, strip) where

encode :: String -> String
encode xs = show (length xs) 

dims :: String -> (Int, Int)
dims s = ((ceiling len), (floor len))
    where len = sqrt (fromIntegral (length s))

strip :: String -> Int -> Int -> [String]
strip [] _ _ = []
strip (x:xs) cols col
    | col == cols = strip (x:xs) cols 0
    | col == 0 = [(grab (x:xs) cols cols)] ++ (strip xs cols (col + 1))
    | otherwise = strip xs cols (col + 1)

grab :: String -> Int -> Int -> String
grab [] _ _ = []
grab (x:xs) num ctr
    | ctr == 0 = [] 
    | otherwise = [x] ++ (grab xs num (ctr - 1))

tt :: String -> [String]
tt [] = []
tt (x:xs) = [[x]] ++ [(grab xs 2 2)]
 
-- square root of length
-- round up for cols, round down for rows
-- pad with spaces
-- build list of strings
-- 
