module Change (findFewestCoins) where

import Data.List (sort, foldl1)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins target denoms = if null used then Nothing else Just used
    where used = findFewest target $ forEachDenom target (reverse $ sort denoms)

forEachDenom :: Integer -> [Integer] -> [[Integer]]
forEachDenom _ [] = []
forEachDenom target denoms = map' target [] (makeChange target) (setsOf denoms)

makeChange :: Integer -> [Integer] -> [Integer]
makeChange _ [] = []
makeChange target (denom:denoms)
    | target == denom = [denom]
    | target < denom = findFewest target $ forEachDenom target denoms
    | target > denom * 3 && denoms `areAllFactorsOf` denom = denom:(makeChange (target - denom) (denom:denoms))
    | otherwise = denom:(findFewest (target - denom) $ forEachDenom (target - denom) (denom:denoms))

fewest :: [[Integer]] -> [Integer]
fewest [] = []
fewest xss = foldl1 (\acc x -> if length x < length acc then x else acc ) $ xss

findFewest :: Integer -> [[Integer]] -> [Integer]
findFewest target xss =
    fewest filtered
    where filtered = filter (\xs -> (sum xs) == target) xss

setsOf :: [Integer] -> [[Integer]]
setsOf [] = []
setsOf (x:xs) = (x:xs):(setsOf xs)

map' :: Integer -> [Integer] -> ([Integer] -> [Integer]) -> [[Integer]] -> [[Integer]]
map' _ _ _ [] = []
map' target wins f (xs:xss)
    | (head xs) `isFactorOf` wins = map' target wins' f xss
    | otherwise = result:(map' target wins' f xss)
    where result = (f xs )
          success = sum result == target
          wins' = if success then (result ++ wins) else wins

isFactorOf :: Integer -> [Integer] -> Bool
isFactorOf el coll = any (\c -> c `rem` el == 0) coll

areAllFactorsOf :: [Integer] -> Integer -> Bool
areAllFactorsOf denoms denom = all (\x -> denom `rem` x == 0) denoms
