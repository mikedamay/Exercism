module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | n == 1 = Just Deficient
    | result == n = Just Perfect
    | result > n = Just Abundant
    | otherwise = Just Deficient
         where
         includeFactor idx' x' n' = if idx' `mod` 2 == 0 then x' else if n' `div` x' == x' then 0 else n' `div` x'
         maxTrial = floor . sqrt . fromIntegral
         result = 1 + (sum [includeFactor idx x n | x <- [2..(maxTrial n)], idx <- [0 :: Integer ..1], n `mod` x == 0])

