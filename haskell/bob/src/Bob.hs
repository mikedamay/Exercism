module Bob (responseFor) where

import Data.List.Extra (trim)
import Data.Char (isLower, isLetter)

responseFor :: String -> String
responseFor xs
    | null trimmed = "Fine. Be that way!"
    | last trimmed == '?' && isYelling = "Calm down, I know what I'm doing!"
    | last trimmed == '?' = "Sure."
    | isYelling = "Whoa, chill out!"
    | otherwise = "Whatever."
  where
    trimmed = trim xs
    isYelling = not (any isLower trimmed) && any isLetter trimmed