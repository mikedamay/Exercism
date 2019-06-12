module Pangram (isPangram) where

import Data.List (sort, nub)
import Data.Char (isLetter, toLower)

isPangram :: String -> Bool
isPangram text = (prepare text) == alphabet

alphabet :: String
alphabet = ['a'..'z']

prepare :: String -> String
prepare = (nub . sort . map toLower . filter isLetter )