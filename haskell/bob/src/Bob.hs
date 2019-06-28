module Bob (responseFor) where

import Data.List.Extra (trim)
import Data.Char (isUpper, isLower, isLetter)

responseFor :: String -> String
responseFor xs
    | null trimmed = "Fine. Be that way!"
    | last trimmed == '?' && isYelling = "Calm down, I know what I'm doing!"
    | last trimmed == '?' = "Sure."
    | isYelling = "Whoa, chill out!"
    | otherwise = "Whatever."
  where
    trimmed = trim xs
    isYelling2 = any isLetter trimmed && (all isUpper $ filter isLetter trimmed)
    isYelling = not (any isLower trimmed) && any isLetter trimmed