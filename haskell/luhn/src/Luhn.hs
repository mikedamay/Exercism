module Luhn (isValid, towardsLuhn, luhnify) where

import Data.Char (isDigit, digitToInt)

isValid :: String -> Bool
isValid n = (sum $ towardsLuhn n) `mod` 10 == 0

towardsLuhn :: String -> [Integer]
towardsLuhn numStr = convertEveryOtherDigit cleaned
    where cleaned = [c2 | c2 <- numStr, isDigit c2 ]

convertEveryOtherDigit :: String -> [Integer]
convertEveryOtherDigit a = convertEveryOtherDigit' (length a) a

convertEveryOtherDigit' :: Int -> String -> [Integer]
convertEveryOtherDigit' _ [] = []
convertEveryOtherDigit' ctr (x:xs)
    | ctr `mod` 2 == 0 = (makeLuhnDigit n) : (convertEveryOtherDigit' (ctr - 1) xs)
    | otherwise        = n : convertEveryOtherDigit' (ctr - 1) xs
    where n = toInteger $ digitToInt x

makeLuhnDigit :: Integer -> Integer
makeLuhnDigit n = if dub > 9 then dub - 9 else dub
    where dub = n * 2