module Prime (nth) where

nth :: Int -> [Integer]
nth n = snd (nextPrime ((drop 1  [1..100]), []))

nextPrime :: ([Integer], [Integer]) -> ([Integer], [Integer])
nextPrime ([], sieved) = ([], sieved)
nextPrime (cand:candidates, sieved) = nextPrime ([x | x <- candidates, x `mod` cand /= 0], cand : sieved)


