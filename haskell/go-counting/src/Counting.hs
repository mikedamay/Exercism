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
    arr :: ColorArray
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
    cleaned = Set.filter (\coord -> (getColor colors coord) == Nothing) $ territory

boardToColors :: [String] -> Colors
boardToColors board = colors
  where
    colorArray = strToArray $ transpose board
    colors = Colors
                {arr = colorArray
                , getColor = (colorArray Array.!)
                , includes = (isValidCoord colorArray)
                , coordsxx = coords' colorArray
                }

findTerritories :: Colors -> [CoordSet] -> [Coord] -> [CoordSet]
findTerritories _ territories [] = territories
findTerritories colors territories (c:coords)
    | territories `containsCoord` c = findTerritories colors territories coords
    | getColor colors c /= Nothing = findTerritories colors territories coords
    | otherwise = findTerritories colors (set:territories) coords
        where set = queryCell c colors Set.empty

queryCell :: Coord -> Colors -> CoordSet -> CoordSet
queryCell coord colors set
    | set `setContainsCoord` coord = set
    | color /= Nothing = Set.insert coord set
    | otherwise = foldl (\acc c -> queryCell c colors acc) (Set.insert coord set) (neighbours (arr colors) coord)
  where
    color = getColor colors coord
    (maxRow, maxCol) = snd $ Array.bounds (arr colors)

containsCoord :: [CoordSet] -> Coord -> Bool
containsCoord sets coord = any ((flip setContainsCoord) coord) sets

setContainsCoord :: CoordSet -> Coord -> Bool
setContainsCoord set coord = Set.member coord set

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
formatResults colors sets = map (formatResult colors) sets

formatResult :: Colors -> CoordSet -> (CoordSet, Maybe Color)
formatResult colors set | set == Set.empty = (Set.empty, Nothing)
formatResult colors set =
    (Set.fromList $ cleaned, color)
  where
    color = territoryColor colors set
    cleaned = filter (\coord -> (getColor colors coord) == Nothing) $ Set.toList set

territoryColor :: Colors -> CoordSet -> Maybe Color
territoryColor _ set | set == Set.empty = Nothing
territoryColor colors set = case resolve of
                                Nothing -> Nothing
                                Just x -> x
  where
    resolve = foldl resolveColor (Just Nothing) (Set.toList set)
    resolveColor acc c = if acc == Nothing then Nothing else
                               if acc == Just Nothing then Just color else
                                 if color == Nothing then acc else
                                   if Just color == acc then acc else
                                       Nothing
                                     where color = getColor colors c

strToArray :: [String] -> ColorArray
strToArray lines = Array.listArray ((minRow, minCol), (minRow + (length lines) - 1, minCol + (lengthOfLine lines) - 1))
    $ map charToColor $ concat $ lines
  where
    charToColor 'B' = Just Black
    charToColor 'W' = Just White
    charToColor ' ' = Nothing
    charToColor _ = error "Unknown color"
    lengthOfLine [] = 0
    lengthOfLine ([]:xs) = 0
    lengthOfLine (x:xs) = length x
    (minRow, minCol) = (1, 1)

isValidCoord :: ColorArray -> Coord -> Bool
isValidCoord arr (r, c) = r >= minRow && r <= maxRow && c >= minCol && c <= maxCol
  where
    ((minRow, minCol), (maxRow, maxCol)) = Array.bounds arr



allBlank = ["   ", "   "]
allBlankSquare = ["   ", "   ", "   "]
simple =  ["B   B", "B   B"]
square = ["BBBBB", "B   B", "B   B","B   B", "BBBBB"]
twoBlank = ["  "]
main = [ "  B  "
       , " B B "
       , "B W B"
       , " W W "
       , "  W  " ]

setup :: [String] -> (Colors, [Coord])
setup board = (colors, coordsxx colors)
  where
    colors = boardToColors board

