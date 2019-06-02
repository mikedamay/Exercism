module Isogram (isIsogram) where

import Data.Char (isLetter, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram str = (length $ nub clean_str) == (length clean_str)
            where clean_str = map toLower $ filter isLetter str
