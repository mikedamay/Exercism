module Advanced where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate m = zipWith (zipWith toChar) mines adj -- [[True, False, False][1,3,0]] -> ["*30"]
  where
    mines = (map . map) (== '*') m  -- [[False, True, False][...]]
    adj = smooth . (map . map $ fromEnum) $ mines  -- fromEnum True == 1
    toChar True _  = '*'
    toChar False 0 = ' '
    toChar False n = intToDigit n

smooth :: [[Int]] -> [[Int]]
smooth = map (trips add3 0) . trips (zipWith3 add3) (repeat 0)
  where
    add3 a b c = a + b + c

trips :: (a -> a -> a -> b) -> a -> [a] -> [b]
trips f border = go . (++ [border]) . (border :)
  where
    go l@(a:b:c:_) = f a b c : go (tail l)
    go _           = []

collatz :: Integer -> Integer
collatz 1 = 0
collatz n = 1 + (if n `rem` 2 == 0 then collatz (n `div` 2) else collatz (n * 3 + 1))

fc :: Int
fc = length $ filter (>15) $ map collatz [1..100]