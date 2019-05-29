module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
    | n <= 0 = Nothing
    | otherwise = if result == n then Just Perfect
         else if result > n then Just Abundant
         else Just Deficient
        where result = sum [x | x <- [1..n`div`2], n `mod` x == 0]


-- classify :: Int -> Maybe Classification
-- classify n = if (floor (sqrt (fromIntegral n ))) > 10 then Nothing else Nothing