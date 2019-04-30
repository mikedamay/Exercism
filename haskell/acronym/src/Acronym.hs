module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = map (toUpper) (concat [ (significantChar x) | x <- split (" " ++ xs)])

significantChar :: [Char] -> [Char]
significantChar xs = head xs : (if filter (isLower) (tail xs) /= [] then filter (isUpper) (tail xs) else [])


-- significantChars [] = []
-- significantChars (x:[]) = if isUpper [x] then x else []
-- significantChars (x:y:xs) = significantChars y:xs ++ (if (isUpper x) && not (isUpper y) then x else [])

rev [] = []
rev (x:[]) = [x]
rev (x:xs) = (rev xs) ++ [x]

nosp [] = []
nosp (' ':[]) = []
nosp (' ':xs) = nosp(xs)
nosp (x:[]) = [x]
nosp (x:xs) = [x] ++ nosp(xs)


split :: [Char] -> [[Char]]
split [] = []
split (' ':[]) = []
split (' ':' ':xs) = split(xs)
split (' ':'-':xs) = split(xs)
split ('-':' ':xs) = split(xs)
split ('-':'-':xs) = split(xs)
split (' ':xs) = [wd(xs)] ++ split(xs)
split ('-':[]) = []
split ('-':xs) = [wd(xs)] ++ split(xs)
split (x:xs) = [] ++ split(xs)

wd :: [Char] -> [Char]
wd [] = []
wd (' ':[]) = []
wd (' ':xs) = []
wd (x:[]) = [x]
wd (x:xs) = [x] ++ wd(xs)