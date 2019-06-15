module Matrix (saddlePoints) where

import Data.Array (Array, listArray, elems, bounds)

import Data.List (transpose)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = prepared $ getList matrix

tester :: [[Int]]
tester2 = [ [9, 8, 7]
         , [5, 3, 2]
         , [6, 6, 7] ]

tester = [ [10, 20]
         , [40, 30] ]

tester3 :: [[Int]]
tester3 = [ [3, 1, 3]
            , [3, 2, 4] ]

addCoords :: [a] -> [(Int, a)]
addCoords = zip [0 :: Int ..]

addColIds :: [(Int, [Int])] -> [(Int, [(Int, Int)])]
addColIds = map addColId

addColId :: (Int, [Int]) -> (Int, [(Int, Int)])
addColId (r, l) = (r, zip [0..] l)

coordinated :: [[Int]] -> [[(Int, Int, Int)]]
coordinated xs = map addRowIds $ addColIds $ addCoords xs

prepared xs = map (\(r, c, v) -> (r, c)) $ concat $ defs [(x, y) | x <- maxMap $ coordinated xs, y <- minMap $ transpose $ coordinated xs ]

addRowIds :: (Int, [(Int, Int)]) -> [(Int, Int, Int)]
addRowIds (r, xs) = map (addRowId r) xs

addRowId :: Int -> (Int, Int) -> (Int, Int, Int)
addRowId r (c, v) = (r, c, v)

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

abc = [([(0,1,20)],[(0,0,10)]),([(0,1,20)],[(0,1,20)]),([(1,0,40)],[(0,0,10)]),([(1,0,40)],[(0,1,20)])]

defs :: [([(Int, Int, Int)],[(Int, Int, Int)])] -> [([(Int, Int, Int)])]
defs = map def

def :: ([(Int, Int, Int)], [(Int, Int, Int)]) -> ([(Int, Int, Int)])
def (rows, cols) = [ r | r <- rows, c <- cols, r == c]

two = [([(0,2,3),(0,0,3)],[(1,0,3),(0,0,3)]),([(0,2,3),(0,0,3)],[(0,1,1)]),([(0,2,3),(0,0,3)],[(0,2,3)]),([(1,2,4)],[(1,0,3),(0,0,3)]),([(1,2,4)],[(0,1,1)]),([(1,2,4)],[(0,2,3)])] :: [([(Int, Int, Int)],[(Int, Int, Int)])]

matrixFromList xss =
    matrix
    where
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)

getNumCols :: (Array (Int, Int) Int) -> Int
getNumCols  m = fromIntegral $ (+) 1 $ snd $ snd $ bounds m

getList :: (Array (Int, Int) Int) -> [[Int]]
getList m = myChunksOf (getNumCols m) $ elems m

myChunksOf :: Int -> [Int] -> [[Int]]
myChunksOf _ [] = []
myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))