module Luhn (isValid, towardsLuhn, luhnify, removeEveryOther, twiddle) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = (sum $ towardsLuhn n) `mod` 10 == 0

towardsLuhn :: String -> [Integer]
towardsLuhn numStr = removeEveryOther cleaned
    where cleaned = [c2 | c2 <- numStr, isDigit c2 ]

luhnify :: Char -> [Integer]
luhnify c =
    let n = toInteger $ digitToInt c in
    [n, ((n*2) `mod` 10)]


removeEveryOther :: String -> [Integer]
removeEveryOther a = removeEveryOther' (length a) a

removeEveryOther' :: Int -> String -> [Integer]
removeEveryOther' _ [] = []
removeEveryOther' ctr (x:xs)
    | ctr `mod` 2 == 0 = (twiddle n) : (removeEveryOther' (ctr - 1) xs)
    | otherwise        = n : removeEveryOther' (ctr - 1) xs
    where n = toInteger $ digitToInt x

twiddle :: Integer -> Integer
twiddle n = if dub > 9 then dub - 9 else dub
    where dub = n * 2