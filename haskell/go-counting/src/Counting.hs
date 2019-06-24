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


findTerritories :: Array.Array (Int, Int) Color -> [Set.Set (Int, Int, Color)] -> [(Int, Int)] -> [Set.Set (Int, Int, Color)]
findTerritories _ territories [] = territories
findTerritories arr territories (c:coords)
    | territories `containsCoord` c = findTerritories arr territories coords
    | (arr Array.! c) /= None = findTerritories arr territories coords
    | otherwise = findTerritories arr (set:territories) coords
        where set = queryCell c arr Set.empty

-- queryCell :: (Int, Int) -> Array.Array (Int, Int) Color -> Set.Set (Int, Int, Color) -> Set.Set (Int, Int, Color)
-- queryCell coord@(r, c) arr set
--     | set `containsCoord` coord = set
--     | color /= None = S.insert (r, c, color) set
--     | otherwise
--   where
--     color = arr Array.! coord

queryCell (r, c) arr set = Set.insert (r, c, None) set

containsCoord :: [Set.Set (Int, Int, Color)] -> (Int, Int) -> Bool
containsCoord sets (r, c) = any (\set -> Set.member (r, c, Black) set || Set.member (r, c, White) set || Set.member (r, c, None) set  ) sets


coords :: Array.Array (Int, Int) Color -> [(Int, Int)]
coords arr = [(r, c) | r <- [0..maxRow], c <- [0..maxCol]]
  where
    (min, (maxRow, maxCol)) = Array.bounds arr

polishTerritories :: [Set.Set (Int, Int, Color)] -> [(Set.Set (Int, Int), Maybe Color)]
polishTerritories [] = []
polishTerritories sets = map polishTerritory sets

polishTerritory :: Set.Set (Int, Int, Color) -> (Set.Set (Int, Int), Maybe Color)
polishTerritory set | set == Set.empty = (Set.empty, Nothing)
polishTerritory set =
    (Set.fromList $ [(x, y) | (x, y, c) <- cleaned], if color == Just None then Nothing else color)
  where
    color = territoryColor set
    cleaned = filter (\(x, y, c) -> c == None) $ Set.toList set

territoryColor :: Set.Set (Int, Int, Color) -> Maybe Color
territoryColor set | set == Set.empty = Nothing
territoryColor set = foldl (\acc (x, y, c) -> if acc == Nothing then Nothing else
                                                if acc == Just None then Just c else
                                                    if c == None then acc else
                                                        if Just c == acc then acc else
                                                            Nothing
                                                        ) (Just None) (Set.toList set)

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

filterPrimes (p:xs) =
    p : (filterPrimes [x | x <- xs, x `rem` p /= 0])