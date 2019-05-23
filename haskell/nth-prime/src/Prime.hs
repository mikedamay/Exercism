module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (sieve [2..] (n - 1))

sieve :: [Integer] -> Int -> Integer
sieve [] _ = 0 -- avoid warning
sieve (cand:candidates) ctr
    | ctr == 0 = cand
    | otherwise = sieve [x | x <- candidates, x `mod` cand /= 0] (ctr - 1)

