module Counting (
    Color(..),
    territories,
    territoryFor,
) where

import qualified Data.Set as Set
import qualified Data.Array as Array

data Color = Black | White | None deriving (Eq, Ord, Show)
type Coord = (Int, Int)
type CoordAndColor = (Int, Int, Color)

type ColorArray = Array.Array (Int, Int) Color
data Colors = Colors {arr :: ColorArray, getColor :: Coord -> Color }

type CoordAndColorSet = Set.Set (Int, Int, Color)
type CoordSet = Set.Set (Int, Int)

minRow :: Int
minRow = 1

minCol :: Int
minCol = 1

territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board = polishTerritories colors $ findTerritories' colors [] coo
  where
    colorArray = strToArray board
    coo = coords colorArray
    colors = Colors {arr = colorArray, getColor = (colorArray Array.!)}

territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = error "You need to implement this function."

setup :: (Colors, [Coord])
setup = (colors, coo)
  where
--     arr = strToArray ["   ", "   "]
    arr = strToArray ["B   B", "B   B"]
    coo = coords arr
    colors = Colors {arr = arr, getColor = (arr Array.!)}

start :: ColorArray -> [CoordSet]
start arr = findTerritories' colors [] []
  where
    colors = Colors {arr = arr, getColor = (arr Array.!)}

findTerritories' :: Colors -> [CoordSet] -> [Coord] -> [CoordSet]
findTerritories' _ territories [] = territories
findTerritories' colors territories (c:coords)
    | territories `containsCoord'` c = findTerritories' colors territories coords
    | getColor colors c /= None = findTerritories' colors territories coords
    | otherwise = findTerritories' colors (set:territories) coords
        where set = queryCell' 5 c colors Set.empty

queryCell' :: Int -> Coord -> Colors -> CoordSet -> CoordSet
queryCell' ctr coord colors set
    | ctr == 0 = set
    | set `setContainsCoord'` coord = set
    | color /= None = Set.insert coord set
    | otherwise = foldl (\acc c -> queryCell' (ctr - 1) c colors acc) (Set.insert coord set) (neighbours maxCol maxRow coord)
  where
    color = getColor colors coord
    (maxCol, maxRow) = snd $ Array.bounds (arr colors)

queryStuff :: Int -> Coord -> Colors -> [Coord] -> [Coord]
queryStuff ctr coord colors set
    | ctr == 0 = set
    | coord `elem` set = set
    | color /= None = coord:set
    | otherwise = foldl (\acc c -> queryStuff (ctr - 1) c colors acc) (coord:set) (neighbours maxCol maxRow coord)
  where
    color = getColor colors coord
    (maxCol, maxRow) = snd $ Array.bounds (arr colors)


containsCoord' :: [CoordSet] -> Coord -> Bool
containsCoord' sets coord = any ((flip setContainsCoord') coord) sets

setContainsCoord' :: CoordSet -> Coord -> Bool
setContainsCoord' set coord = Set.member coord set

addCoords :: Coord -> Coord -> Coord
addCoords (c1, r1) (c2, r2) = (c1 + c2, r1 + r2)

neighbours :: Int -> Int -> Coord -> [Coord]
neighbours maxCol maxRow coord = filter (\(c, r) -> r >= minRow && r <= maxRow && c >= minCol && c <= maxCol) $ map (addCoords coord) [(0, 1), (1, 0), (0, -1), (-1, 0)]

coords :: ColorArray -> [Coord]
coords arr = [(c, r) | c <- [minCol..maxCol], r <- [minRow..maxRow]]
  where
    (min, (maxCol, maxRow)) = Array.bounds arr

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
territoryColor colors set = foldl (\acc coord -> if acc == Nothing then Nothing else
                                                   if acc == Just None then Just (getColor colors coord) else
                                                     if getColor colors coord == None then acc else
                                                       if Just (getColor colors coord) == acc then acc else
                                                            Nothing
                                                        ) (Just None) (Set.toList set)

strToArray :: [String] -> ColorArray
strToArray lines = Array.listArray ((minCol, minRow), (minCol + (lengthOfLine lines) - 1, minRow + (length lines) - 1))
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

jon =  ((Array.listArray ((0,0),(0,1)) [Black, Black]) Array.!)

bob = Colors {arr = Array.listArray ((0,0),(0,1)) [Black, Black], getColor = jon}
