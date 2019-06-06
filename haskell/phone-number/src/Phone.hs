module Phone (number) where

import Data.Char (isDigit, digitToInt)

number :: String -> Maybe String
number pn
    | not $ isValid = Nothing
    | otherwise = Just cleaned
    where cleaned = queryStrip [x | x <- pn, isDigit x ]
          queryStrip (x:xs) = if x == '1' then xs else x:xs
          queryStrip _ = error "in the wrong place"
          isValid = length cleaned == 10 && digitToInt (cleaned !! 0) >= 2
                                         && digitToInt (cleaned !! 3) >= 2
