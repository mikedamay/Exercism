module Counting (
    Color(..),
    territories,
    territoryFor,
) where

import Data.List
import qualified Data.Set as Set
import qualified Data.Array as Array
import Data.Tuple (swap)

data Color = Black | White | None deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type CoordAndColor = (Int, Int, Color)

type ColorArray = Array.Array (Int, Int) Color
data Colors = Colors {
    arr :: ColorArray
    , getColor :: Coord -> Color
    , includes :: Coord -> Bool
    , coordsxx :: [(Int, Int)]
    }

type CoordAndColorSet = Set.Set (Int, Int, Color)
type CoordSet = Set.Set (Int, Int)

minRow :: Int
minRow = 1

minCol :: Int
minCol = 1

territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board = map normalise $ reverse $ polishTerritories colors $ findTerritories colors [] (coordsxx colors)
  where
    colors = boardToColors board

territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = if  not (colors `includes` coord) || null cleaned then Nothing else Just (normalise $ polishTerritory colors territory)
  where
    colors = boardToColors board
    territory = queryCell 5 (swap coord) colors Set.empty
    cleaned = filter (\coord -> (getColor colors coord) == None) $ Set.toList territory

boardToColors :: [String] -> Colors
boardToColors board = colors
  where
    colorArray = strToArray board
    colors = Colors
                {arr = colorArray
                , getColor = (colorArray Array.!)
                , includes = (isValidCoord colorArray)
                , coordsxx = coords' colorArray
                }



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

findTerritories :: Colors -> [CoordSet] -> [Coord] -> [CoordSet]
findTerritories _ territories [] = territories
findTerritories colors territories (c:coords)
    | territories `containsCoord` c = findTerritories colors territories coords
    | getColor colors c /= None = findTerritories colors territories coords
    | otherwise = findTerritories colors (set:territories) coords
        where set = queryCell 5 c colors Set.empty

queryCell :: Int -> Coord -> Colors -> CoordSet -> CoordSet
queryCell ctr coord colors set
    | ctr == 0 = set
    | set `setContainsCoord` coord = set
    | color /= None = Set.insert coord set
    | otherwise = foldl (\acc c -> queryCell (ctr - 1) c colors acc) (Set.insert coord set) (neighbours (arr colors) coord)
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

polishTerritories :: Colors -> [CoordSet] -> [(CoordSet, Maybe Color)]
polishTerritories _ [] = []
polishTerritories colors sets = map (polishTerritory colors) sets

polishTerritory :: Colors -> CoordSet -> (CoordSet, Maybe Color)
polishTerritory colors set | set == Set.empty = (Set.empty, Nothing)
polishTerritory colors set =
    (Set.fromList $ cleaned, if color == Just None then Nothing else color)
  where
    color = territoryColor colors set
    cleaned = filter (\coord -> (getColor colors coord) == None) $ Set.toList set

territoryColor :: Colors -> CoordSet -> Maybe Color
territoryColor _ set | set == Set.empty = Nothing
territoryColor colors set = foldl resolveColor (Just None) (Set.toList set)
  where resolveColor acc c = if acc == Nothing then Nothing else
                               if acc == Just None then Just color else
                                 if color == None then acc else
                                   if Just color == acc then acc else
                                       Nothing
                                     where color = getColor colors c

strToArray :: [String] -> ColorArray
strToArray lines = Array.listArray ((minRow, minCol), (minRow + (length lines) - 1, minCol + (lengthOfLine lines) - 1))
    $ map charToColor $ concat $ lines
  where
    charToColor 'B' = Black
    charToColor 'W' = White
    charToColor ' ' = None
    charToColor _ = error "Unknown color"
    lengthOfLine [] = 0
    lengthOfLine ([]:xs) = 0
    lengthOfLine (x:xs) = length x
    (minRow, minCol) = (1, 1)

normalise :: (CoordSet, Maybe Color) -> (CoordSet, Maybe Color)
normalise (set, color) = (Set.fromList $ map swap $ Set.toList set, color)

isValidCoord :: ColorArray -> Coord -> Bool
isValidCoord arr (r, c) = r >= minRow && r <= maxRow && c >= minCol && c <= maxCol
  where
    ((minRow, minCol), (maxRow, maxCol)) = Array.bounds arr
