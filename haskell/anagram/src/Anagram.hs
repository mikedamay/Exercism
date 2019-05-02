module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss =
    let
        lwrWord = map toLower xs
    in
    [ x | x <- (excludeWord lwrWord xss), (sort (map toLower x)) == (sort lwrWord)]

excludeWord :: String -> [String]  -> [String]
excludeWord xs xss = [x | x <- xss, (map toLower x) /= (map toLower xs) ]
