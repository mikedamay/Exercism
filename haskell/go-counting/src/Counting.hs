module Counting (
    Color(..),
    territories,
    territoryFor,
) where

import qualified Data.Set as Set
import qualified Data.Array as Array

data Color = Black | White | None deriving (Eq, Ord, Show)
type Coord = (Int, Int)

territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = error "You need to implement this function."


findTerritories :: Array.Array (Int, Int) Color -> [Set.Set (Int, Int, Color)]
findTerritories arr = [Set.empty]

-- polishTerritories :: [Set.Set (Int, Int, Color)] -> [(Set (Int, Int), Color)]
-- polishTerritories [] = []
-- polishTerritories sets
--     | length $ nub $ filter (\(x, y, c) -> c /= None) > 1 sets =
--
polishTerritory :: Set.Set (Int, Int, Color) -> (Set.Set (Int, Int), Maybe Color)
polishTerritory set | set == Set.empty = (Set.empty, Nothing)
polishTerritory set =
    (Set.fromList $ [(x, y) | (x, y, c) <- (Set.toList set)], territoryColor set)

territoryColor :: Set.Set (Int, Int, Color) -> Maybe Color
territoryColor set | set == Set.empty = Nothing
territoryColor set = foldl (\acc (x, y, c) -> if acc == Nothing then Nothing else
                                                if acc == Just None then Just c else
                                                    if c == None then acc else
                                                        if Just c == acc then acc else
                                                            Nothing
                                                        ) (Just None)  (Set.toList set)

stuff :: Set.Set (Int, Int, Color) -> [(Int, Int, Color)]
stuff set = Set.toList set

strToArray :: [String] -> Array.Array (Int, Int) Color
strToArray lines = Array.listArray ((0, 0), (length lines - 1, lengthOfLine lines - 1))
    $ map charToColor $ concat lines
  where
    charToColor 'B' = Black
    charToColor 'W' = White
    charToColor ' ' = None
    charToColor _ = error "Unknown color"
    lengthOfLine [] = 0
    lengthOfLine ([]:xs) = 0
    lengthOfLine (x:xs) = length x

