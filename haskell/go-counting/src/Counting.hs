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

territories :: [String] -> [(Set.Set Coord, Maybe Color)]
territories board = error "You need to implement this function."

territoryFor :: [String] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = error "You need to implement this function."


findTerritories :: ColorArray -> [CoordAndColorSet] -> [Coord] -> [CoordAndColorSet]
findTerritories _ territories [] = territories
findTerritories arr territories (c:coords)
    | territories `containsCoord` c = findTerritories arr territories coords
    | (arr Array.! c) /= None = findTerritories arr territories coords
    | otherwise = findTerritories arr (set:territories) coords
        where set = queryCell c arr Set.empty
              colors = Colors {arr = arr, getColor = (arr Array.!)}

setup :: (Colors, [Coord])
setup = (colors, coo)
  where
    arr = strToArray ["   ", "   "]
    coo = coords arr
    colors = Colors {arr = arr, getColor = (arr Array.!)}

start :: ColorArray -> [CoordAndColorSet]
start arr = findTerritories' colors [] []
  where
    colors = Colors {arr = arr, getColor = (arr Array.!)}

findTerritories' :: Colors -> [CoordAndColorSet] -> [Coord] -> [CoordAndColorSet]
findTerritories' _ territories [] = territories
findTerritories' arr territories (c:coords)
    | territories `containsCoord` c = findTerritories' arr territories coords
    | getColor arr c /= None = findTerritories' arr territories coords
    | otherwise = findTerritories' arr (set:territories) coords
        where set = queryCell' c arr Set.empty

queryCell :: Coord -> ColorArray -> CoordAndColorSet -> CoordAndColorSet
queryCell coord@(r, c) arr set
    | set `setContainsCoord` coord = set
    | color /= None = Set.insert (r, c, color) set
    | otherwise = foldl (\acc x -> queryCell x arr (Set.insert (addColorToCoord arr x) acc)) set (neighbours maxRow maxCol coord)
  where
    color = arr Array.! coord
    (maxRow, maxCol) = snd $ Array.bounds arr

queryCell' :: Coord -> Colors -> CoordAndColorSet -> CoordAndColorSet
queryCell' coord@(r, c) colors set
    | set `setContainsCoord` coord = set
    | color /= None = Set.insert (r, c, color) set
    | otherwise = foldl (\acc x -> queryCell' x colors (Set.insert (addColorToCoord' colors x) acc)) (Set.insert (r, c, None) set) (neighbours maxRow maxCol coord)
  where
    color = getColor colors coord
    (maxRow, maxCol) = snd $ Array.bounds (arr colors)

addColorToCoord :: ColorArray -> Coord -> CoordAndColor
addColorToCoord arr coord@(r, c) = (r, c, arr Array.! coord)

addColorToCoord' :: Colors -> Coord -> CoordAndColor
addColorToCoord' colors coord@(r, c) = (r, c, getColor colors coord)

containsCoord :: [CoordAndColorSet] -> Coord -> Bool
containsCoord sets (r, c) = any (\set -> Set.member (r, c, Black) set || Set.member (r, c, White) set || Set.member (r, c, None) set  ) sets

setContainsCoord :: CoordAndColorSet -> Coord -> Bool
setContainsCoord set (r, c) = Set.member (r, c, Black) set || Set.member (r, c, White) set || Set.member (r, c, None) set

addCoords :: Coord -> Coord -> Coord
addCoords (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)

neighbours :: Int -> Int -> Coord -> [Coord]
neighbours maxRow maxCol coord = filter (\(r, c) -> r >= 0 && r <= maxRow && c >= 0 && c <= maxCol) $ map (addCoords coord) [(1, 0), (0, 1), (-1, 0), (0, -1)]

coords :: ColorArray -> [Coord]
coords arr = [(r, c) | r <- [0..maxRow], c <- [0..maxCol]]
  where
    (min, (maxRow, maxCol)) = Array.bounds arr

polishTerritories :: [CoordAndColorSet] -> [(CoordSet, Maybe Color)]
polishTerritories [] = []
polishTerritories sets = map polishTerritory sets

polishTerritory :: CoordAndColorSet -> (CoordSet, Maybe Color)
polishTerritory set | set == Set.empty = (Set.empty, Nothing)
polishTerritory set =
    (Set.fromList $ [(x, y) | (x, y, c) <- cleaned], if color == Just None then Nothing else color)
  where
    color = territoryColor set
    cleaned = filter (\(x, y, c) -> c == None) $ Set.toList set

territoryColor :: CoordAndColorSet -> Maybe Color
territoryColor set | set == Set.empty = Nothing
territoryColor set = foldl (\acc (x, y, c) -> if acc == Nothing then Nothing else
                                                if acc == Just None then Just c else
                                                    if c == None then acc else
                                                        if Just c == acc then acc else
                                                            Nothing
                                                        ) (Just None) (Set.toList set)

strToArray :: [String] -> ColorArray
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

jon =  ((Array.listArray ((0,0),(0,1)) [Black, Black]) Array.!)

bob = Colors {arr = Array.listArray ((0,0),(0,1)) [Black, Black], getColor = jon}
