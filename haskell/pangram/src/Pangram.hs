module Pangram (isPangram) where

import Data.Set as Set (fromList, isSubsetOf)
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = fromList alphabet `isSubsetOf` (fromList $ map toLower text)
    where alphabet = ['a'..'z']

