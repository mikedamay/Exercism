module WordCount (wordCount) where

import Data.List (sort, foldr1)

wordCount :: String -> [(String, Int)]
wordcount "" = []
wordCount xs = count ((sort . words) xs) []


count :: [String] -> [(String, Int)] -> [(String, Int)]
count [] _ = []
count (x:[]) [] = [(x, 1)]
count (x:xs) [] = count xs [(x, 1)]
count (x:[]) acc@((aw, ac):remainder)
    | x == aw = (aw, ac + 1):remainder
    | otherwise = (x, 1):acc
count (x:xs) acc@((aw, ac):remainder)
    | x == aw = count xs ((aw, ac + 1):remainder)
    | otherwise = count xs ((x, 1):acc)
