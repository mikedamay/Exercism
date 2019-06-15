module Matrix (saddlePoints) where

import Data.Array (Array, listArray, elems, bounds, assocs)
import Data.List (transpose)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = prepared $ listFromMatrix matrix

abc = [([(0,1,20)],[(0,0,10)]),([(0,1,20)],[(0,1,20)]),([(1,0,40)],[(0,0,10)]),([(1,0,40)],[(0,1,20)])]
two = [([(0,2,3),(0,0,3)],[(1,0,3),(0,0,3)]),([(0,2,3),(0,0,3)],[(0,1,1)]),([(0,2,3),(0,0,3)],[(0,2,3)]),([(1,2,4)],[(1,0,3),(0,0,3)]),([(1,2,4)],[(0,1,1)]),([(1,2,4)],[(0,2,3)])] :: [([(Int, Int, Int)],[(Int, Int, Int)])]

tester :: [[Int]]
tester2 :: [[Int]]
tester2 = [ [9, 8, 7]
         , [5, 3, 2]
         , [6, 6, 7] ]

tester = [ [10, 20]
         , [40, 30] ]

tester3 :: [[Int]]
tester3 = [ [3, 1, 3]
            , [3, 2, 4] ]



prepared :: [[(Int, Int, Int)]] -> [(Int, Int)]
prepared xs = map (\(r, c, v) -> (r, c)) $ concat $ defs [(x, y) | x <- maxMap xs, y <- minMap $ transpose xs ]

maxMap :: [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
maxMap = map maxes

minMap = map mins

maxes :: [(Int, Int, Int)] -> [(Int, Int, Int)]
maxes = foldl rowMax []

mins :: [(Int, Int, Int)] -> [(Int, Int, Int)]
mins = foldl colMin []

rowMax :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
rowMax [] new = [new]
rowMax acc@((racc, cacc, vacc):xs) new@(r, c, v)
    | v > vacc = [(r, c, v)]
    | v == vacc = new:acc
    | otherwise = acc

colMin :: [(Int, Int, Int)] -> (Int, Int, Int) -> [(Int, Int, Int)]
colMin [] new = [new]
colMin acc@((racc, cacc, vacc):xs) new@(r, c, v)
    | v < vacc = [(r, c, v)]
    | v == vacc = new:acc
    | otherwise = acc

defs :: [([(Int, Int, Int)],[(Int, Int, Int)])] -> [([(Int, Int, Int)])]
defs = map def

def :: ([(Int, Int, Int)], [(Int, Int, Int)]) -> ([(Int, Int, Int)])
def (rows, cols) = [ r | r <- rows, c <- cols, r == c]

matrixFromList xss =
    matrix
    where
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)

listFromMatrix :: (Array (Int, Int) Int) -> [[(Int, Int, Int)]]
listFromMatrix n = myChunksOf (getNumCols n) $ normalise $ assocs n
    where
        getNumCols :: (Array (Int, Int) Int) -> Int
        getNumCols  m = (+) 1 $ snd $ snd $ bounds m
        normalise :: [((Int, Int), Int)] -> [(Int, Int, Int)]
        normalise xs = [(r, c, v) | ((r, c), v) <- xs]
        myChunksOf :: Int -> [a] -> [[a]]
        myChunksOf _ [] = []
        myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))



