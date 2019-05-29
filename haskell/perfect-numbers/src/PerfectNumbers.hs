module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | n == 1 = Just Deficient
    | result == n = Just Perfect
    | result > n = Just Abundant
    | otherwise = Just Deficient
         where result = 1 + (sum [if idx `mod` 2 == 0 then x else if n `div` x == x then 0 else n `div` x | x <- [2..(fromIntegral (floor (sqrt (fromIntegral n))))], idx <- [0..1], n `mod` x == 0])

