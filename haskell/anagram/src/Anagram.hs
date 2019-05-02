module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ x | x <- (excludeWord xs xss), (sort (map toLower x)) == (sort (map toLower xs))]

excludeWord :: String -> [String]  -> [String]
excludeWord xs xss = [x | x <- xss, (map toLower x) /= (map toLower xs) ]
