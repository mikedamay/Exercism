module Counting (
    Color(..),
    territories,
    territoryFor,
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Array as Array

data Color = Black | White deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type ColorArray = Array.Array (Int, Int) (Maybe Color)
data Colors = Colors {
    getArrayOf :: ColorArray
    , getColor :: Coord -> Maybe Color
    , includes :: Coord -> Bool
    , coordsxx :: [(Int, Int)]
    }
type CoordSet = Set.Set (Int, Int)

territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board = formatResults colors $ findTerritories colors [] (coordsxx colors)
  where
    colors = boardToColors board

territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = if not (colors `includes` coord) || null cleaned then Nothing else Just (formatResult colors territory)
  where
    colors = boardToColors board
    territory = queryCell coord colors Set.empty
    cleaned = Set.filter (\c -> (getColor colors c) == Nothing) $ territory

boardToColors :: [String] -> Colors
boardToColors board = colors
  where
    colorArray = strToArray $ transpose board
    colors = Colors
                {getArrayOf = colorArray
                , getColor = (colorArray Array.!)
                , includes = (isValidCoord colorArray)
                , coordsxx = coords' colorArray
                }

findTerritories :: Colors -> [CoordSet] -> [Coord] -> [CoordSet]
findTerritories _ territorys [] = territorys
findTerritories colors territorys (c:coords)
    | territorys `containsCoord` c = findTerritories colors territorys coords
    | getColor colors c /= Nothing = findTerritories colors territorys coords
    | otherwise = findTerritories colors (territory:territorys) coords
        where territory = queryCell c colors Set.empty

queryCell :: Coord -> Colors -> CoordSet -> CoordSet
queryCell coord colors territory
    | territory `setContainsCoord` coord = territory
    | color /= Nothing = Set.insert coord territory
    | otherwise = foldl (\acc c -> queryCell c colors acc) (Set.insert coord territory) (neighbours (getArrayOf colors) coord)
  where
    color = getColor colors coord

containsCoord :: [CoordSet] -> Coord -> Bool
containsCoord territorys coord = any ((flip setContainsCoord) coord) territorys

setContainsCoord :: CoordSet -> Coord -> Bool
setContainsCoord territory coord = Set.member coord territory

neighbours :: ColorArray -> Coord -> [Coord]
neighbours colors coord = filter (isValidCoord colors) $ map (addCoords coord) [(0, 1), (1, 0), (0, -1), (-1, 0)]
  where
    addCoords (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

coords' :: ColorArray -> [Coord]
coords' arr = [(r, c) | r <- [minRow..maxRow], c <- [minCol..maxCol]]
  where
    ((minRow, minCol), (maxRow, maxCol)) = Array.bounds arr

formatResults :: Colors -> [CoordSet] -> [(CoordSet, Maybe Color)]
formatResults _ [] = []
formatResults colors territorys = map (formatResult colors) territorys

formatResult :: Colors -> CoordSet -> (CoordSet, Maybe Color)
formatResult _ territory | territory == Set.empty = (Set.empty, Nothing)
formatResult colors territory =
    (Set.fromList $ cleaned, color)
  where
    color = territoryColor colors territory
    cleaned = filter (\coord -> (getColor colors coord) == Nothing) $ Set.toList territory

territoryColor :: Colors -> CoordSet -> Maybe Color
territoryColor _ territory | territory == Set.empty = Nothing
territoryColor colors territory = case resolve of
                                Nothing -> Nothing
                                Just x -> x
  where
    resolve = foldl resolveColor (Just Nothing) (Set.toList territory)
    resolveColor acc c = if acc == Nothing then Nothing else
                               if acc == Just Nothing then Just color else
                                 if color == Nothing then acc else
                                   if Just color == acc then acc else
                                       Nothing
                                     where color = getColor colors c

strToArray :: [String] -> ColorArray
strToArray ll = Array.listArray ((minRow, minCol), (minRow + (length ll) - 1, minCol + (lengthOfLine ll) - 1))
    $ map charToColor $ concat $ ll
  where
    charToColor 'B' = Just Black
    charToColor 'W' = Just White
    charToColor ' ' = Nothing
    charToColor _ = error "Unknown color"
    lengthOfLine [] = 0
    lengthOfLine ([]:_) = 0
    lengthOfLine (x:_) = length x
    (minRow, minCol) = (1, 1)

isValidCoord :: ColorArray -> Coord -> Bool
isValidCoord arr (r, c) = r >= minRow && r <= maxRow && c >= minCol && c <= maxCol
  where
    ((minRow, minCol), (maxRow, maxCol)) = Array.bounds arr

