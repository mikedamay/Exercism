module Luhn (isValid, towardsLuhn, luhnify) where

import Data.Char (isDigit, digitToInt, isSpace)

isValid :: String -> Bool
isValid n
    | (length (removeSpaces n)) <= 1 = False
    | otherwise = (sum $ towardsLuhn n) `mod` 10 == 0

towardsLuhn :: String -> [Integer]
towardsLuhn numStr = [ luhnify x | x <- (zip (map (toInteger . digitToInt) cleaned) [len_cleaned, (len_cleaned - 1)..0])]
    where cleaned = [c2 | c2 <- numStr, isDigit c2 ]
          len_cleaned = toInteger (length cleaned)

luhnify :: (Integer, Integer) -> Integer
luhnify (val, idx)
    | idx `mod` 2 == 0 = if dub > 9 then dub - 9 else dub
    | otherwise = val
    where dub = val * 2

removeSpaces :: String -> String
removeSpaces s = [c | c <- s, not (isSpace c)]

