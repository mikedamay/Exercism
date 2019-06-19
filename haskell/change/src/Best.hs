(findFewestCoins) where

import Data.Array.IArray ((!), Array, array)
import Data.Function (on)
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target _ | target < 0 = Nothing
findFewestCoins target coins = best ! target
  where best :: Array Integer (Maybe [Integer])
        best = array (0, target) ((0, Just []) : [(i, entry i) | i <- [1 .. target]])
        entry i = minByLength (mapMaybe (tryCoin i) coins)
        tryCoin target' c | c > target' = Nothing
        tryCoin target' c = fmap (c:) (best ! (target' - c))

minByLength :: [[a]] -> Maybe [a]
minByLength [] = Nothing
minByLength l  = Just (minimumBy (compare `on` length) l)
