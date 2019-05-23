module Prime (nth) where

nth :: Int -> [Integer]
nth n = nextPrime (drop 1  [1..100])

nextPrime :: [Integer] -> [Integer]
nextPrime [] = []
nextPrime (cand:candidates) = cand : (nextPrime [x | x <- candidates, x `mod` cand /= 0])


