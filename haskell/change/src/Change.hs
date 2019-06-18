module Change (findFewestCoins) where

import Data.List (sort, foldl1)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins 0 _ = Just []
findFewestCoins target coins = if null used then Nothing else Just used
    where used = doMax target $ doStuff target (reverse $ sort coins)

doStuff :: Integer -> [Integer] -> [[Integer]]
doStuff _ [] = []
doStuff target xs = map (makeChange target) (setsOf xs)

makeChange :: Integer -> [Integer] -> [Integer]
makeChange _ [] = []
makeChange target (x:xs)
    | target == x = [x]
    | target < x = doMax target $ doStuff target xs
    | otherwise = x:(doMax (target - x) $ doStuff (target - x) (x:xs))

getMax :: Integer -> [[Integer]] -> [Integer]
getMax _ [] = []
getMax target xss = foldl1 (\acc x -> if length x < length acc then x else acc ) $ xss

setsOf :: [Integer] -> [[Integer]]
setsOf [] = []
setsOf (x:xs) = (x:xs):(setsOf xs)

doMax :: Integer -> [[Integer]] -> [Integer]
doMax target xss =
    getMax target filtered
    where filtered = filter (\xs -> (sum xs) == target) xss