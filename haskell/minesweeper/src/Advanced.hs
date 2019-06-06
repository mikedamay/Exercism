module Advanced where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate m = zipWith (zipWith toChar) mines adj -- [[True, False, False][1,3,0]] -> ["*30"]
  where
    mines = (map . map) (== '*') m  -- [[False, True, False][...]]
    adj = smooth . (map . map $ fromEnum) $ mines  -- fromEnum True == 1 / passed to smooth == [[0, 1, 0][..]]
    toChar True _  = '*'
    toChar False 0 = ' '
    toChar False n = intToDigit n

-- smooth takes each set of 3 lines and combines them vertically to pick up neighbours.m
-- it adds a border line at top and bottom and then concatenates each column vertically
-- then for each line it does the same thing horizontally
smooth :: [[Int]] -> [[Int]]
smooth = map (trips add3 0) . trips (zipWith3 add3) (repeat 0)
  where
    add3 a b c = a + b + c

trips :: (a -> a -> a -> b) -> a -> [a] -> [b]
trips f border = go . (++ [border]) . (border :)
  where
    go l@(a:b:c:_) = f a b c : go (tail l)      -- uses the pattern to ensure only n - 1 items are processed
    go _           = []

add3 a b c = a + b + c

gogo l@(a:b:c:_) = add3 a b c : gogo (tail l)
gogo _ = []

-- trips
-- ist call
-- f is add3, border is

-- this appears to add 0 to the front and back of each list and then add together each set of 3
-- [1, 0] -> [0, 1, 0, 0] -> [1, 1, 1, 0]

-- 2nd call