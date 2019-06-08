module WordCount (wordCount) where

import Data.List (sort, foldr1)
import Data.Char (isAlphaNum, toLower)

wordCount :: String -> [(String, Int)]
wordcount "" = []
wordCount xs = count (sort (mywords (map toLower $ reverse xs))) []


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

mywords :: String -> [String]
mywords all = map mytrim (mywords'' (reverse all))
    where mywords'' = (fst . foldr gather ([], False))
-- mywords'' :: String -> [String]


gather :: Char -> ([String], Bool) -> ([String], Bool)
gather x ([], _) = if isValidChar x then ([[x]], False) else ([], False)
gather x ((y:ys), newLine)
    | isValidChar x && newLine = ([x]:(y:ys), False)
    | isValidChar x = ((x:y):ys, False)
    | otherwise = (y:ys, True)


isValidChar x = isAlphaNum x || x == '\'' || x == '"'

mytrim :: String -> String
mytrim = (dropWhile (== '\'') ) . trimEnd

trimEnd = foldr (\x xs -> if null xs && x == '\'' then [] else x:xs) []

