module Matrix (saddlePoints) where

import Data.Array (Array, listArray, elems, bounds, assocs)
import Data.List (transpose)

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

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = prepared $ listFromMatrix matrix

prepared :: [[((Int, Int), Int)]] -> [(Int, Int)]
prepared xs = map (\((r, c), v) -> (r, c)) $ concat $ defs [(x, y) | x <- maxMap xs, y <- minMap $ transpose xs ]

defs :: [([((Int, Int), Int)],[((Int, Int), Int)])] -> [([((Int, Int), Int)])]
defs = map def

def :: ([((Int, Int), Int)], [((Int, Int), Int)]) -> ([((Int, Int), Int)])
def (rows, cols) = [ r | r <- rows, c <- cols, r == c]

maxMap :: [[((Int, Int), Int)]] -> [[((Int, Int), Int)]]
maxMap = map (foldl findMax [])
    where findMax = saddlePoint (>)

minMap :: [[((Int, Int), Int)]] -> [[((Int, Int), Int)]]
minMap = map (foldl findMin [])
    where findMin = saddlePoint (<)

saddlePoint :: (Int -> Int -> Bool) -> [((Int, Int), Int)] -> ((Int, Int), Int) -> [((Int, Int), Int)]
saddlePoint fc [] new = [new]
saddlePoint fc acc@(((racc, cacc), vacc):xs) new@((r, c), v)
    | fc v vacc = [((r, c), v)]
    | v == vacc = new:acc
    | otherwise = acc

listFromMatrix :: (Array (Int, Int) Int) -> [[((Int, Int), Int)]]
listFromMatrix n = myChunksOf (getNumCols n) $ assocs n
    where
        getNumCols :: (Array (Int, Int) Int) -> Int
        getNumCols  m = (+) 1 $ snd $ snd $ bounds m
        myChunksOf :: Int -> [a] -> [[a]]
        myChunksOf _ [] = []
        myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))

matrixFromList xss =
    matrix
    where
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)



