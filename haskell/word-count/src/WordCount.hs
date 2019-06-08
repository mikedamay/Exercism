module WordCount (wordCount) where

import Data.List (sort, group)
import Data.Char (isAlphaNum, toLower)

wordCount :: String -> [(String, Int)]
wordCount "" = []
wordCount xs = count (sort (mywords (map toLower $ reverse xs)))

count :: [String] -> [(String, Int)]
count xs = map (\s@(x:_) -> (x, length s) ) $ group xs

mywords :: String -> [String]
mywords s = map mytrim (mywords' (reverse s))
    where mywords' = (fst . foldr gather ([], False))

gather :: Char -> ([String], Bool) -> ([String], Bool)
gather x ([], _) = if isValidChar x then ([[x]], False) else ([], False)
gather x ((y:ys), newLine)
    | isValidChar x && newLine = ([x]:(y:ys), False)
    | isValidChar x = ((x:y):ys, False)
    | otherwise = (y:ys, True)

isValidChar :: Char -> Bool
isValidChar x = isAlphaNum x || x == '\''

mytrim :: String -> String
mytrim = (dropWhile (== '\'') ) . trimEnd

trimEnd :: String -> String
trimEnd = foldr (\x xs -> if null xs && x == '\'' then [] else x:xs) []

