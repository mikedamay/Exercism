module Prime (nth) where

nth :: Int -> [Integer]
nth n = sieve [2..] n

sieve :: [Integer] -> Int -> [Integer]
sieve (cand:candidates) ctr
    | ctr == 0 = []
    | otherwise = cand : (sieve [x | x <- candidates, x `mod` cand /= 0] (ctr - 1))

{-
nextPrime :: ([Integer], Integer) -> ([Integer], Integer)
nextPrime ([], ctr) = ([], ctr)
nextPrime ((cand:candidates), ctr) = (cand : (nextPrime [x | x <- candidates, x `mod` cand /= 0]), ctr + 1)


sieve :: Integer -> [Integer] -> Integer -> [Integer]
sieve n candidates ctr
    | ctr >= n = []
    | otherwise = sieved ++ (sieve n cands ctr')
        where cands = nextCandidates candidates
              (sieved, ctr') = nextPrime cands ctr

nextCandidates :: [Integer] -> [Integer]
nextCandidates candidates = [start..end]
    where
        start =
-}
