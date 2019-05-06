module Starred where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor word = filter isAnagram
  where
    word' = sort $ map toLower word
    isAnagram = (== word') . sort . map toLower