module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = map (toUpper) (concat [ (significantChar x) | x <- split (" " ++ xs)])

significantChar :: [Char] -> [Char]
significantChar xs = head xs : (if filter (isLower) (tail xs) /= [] then filter (isUpper) (tail xs) else [])


isSeparator :: Char -> Bool
isSeparator x = x == ' ' || x == '-'

split :: [Char] -> [[Char]]
split [] = []
split (x:y:xs)
    | (isSeparator x) && (isSeparator y) = split (y:xs)
    | (isSeparator y) = split (y:xs)
    | (isSeparator x) = [wd (y:xs)] ++ split (y:xs)
    | otherwise = split xs
split (x:xs)
    | (isSeparator x) = [wd xs] ++ split xs
    | otherwise = split xs


wd :: [Char] -> [Char]
wd [] = []
wd (x:xs)
  | isSeparator x = []
  | otherwise = [x] ++ wd xs
