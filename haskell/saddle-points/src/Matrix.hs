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

data MatrixElement e = MatrixElement ((Int, Int), e) deriving (Eq)

-- -- saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
-- -- saddlePoints matrix = error ""
-- --
saddlePoints :: (Ord e, Eq e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = prepare $ listFromMatrix matrix

prepare :: (Ord e, Eq e) => [[MatrixElement e]] -> [(Int, Int)]
prepare xs = map (\(MatrixElement ((r, c), v)) -> (r, c)) $ concat $ getSaddlePoints [(x, y) | x <- getMaxElements xs, y <- getMinElements $ transpose xs ]

getSaddlePoints :: (Ord e, Eq e) => [([MatrixElement e],[MatrixElement e])] -> [([MatrixElement e])]
getSaddlePoints = map getSaddlePoint
    where
        getSaddlePoint :: (Ord e, Eq e) => ([MatrixElement e], [MatrixElement e]) -> ([MatrixElement e])
        getSaddlePoint (rows, cols) = [ r | r <- rows, c <- cols, r == c]

getMaxElements :: (Ord e) => [[MatrixElement e]] -> [[MatrixElement e]]
getMaxElements = map (foldl findMax [])
    where findMax = saddlePoint (>)

getMinElements :: (Ord e) => [[MatrixElement e]] -> [[MatrixElement e]]
getMinElements = map (foldl findMin [])
    where findMin = saddlePoint (<)

saddlePoint :: (Ord e) => (e -> e -> Bool) -> [MatrixElement e] -> (MatrixElement e) -> [MatrixElement e]
saddlePoint fc [] new = [new]
saddlePoint fc acc@((MatrixElement ((racc, cacc), vacc)):xs) new@(MatrixElement((r, c), v))
    | fc v vacc = [MatrixElement ((r, c), v)]
    | v == vacc = new:acc
    | otherwise = acc

listFromMatrix :: (Ord e) => (Array (Int, Int) e) -> [[MatrixElement e]]
listFromMatrix m = myChunksOf (getNumCols m) $ map elementFromMatrix $ assocs m
    where
        getNumCols :: (Array (Int, Int) e) -> Int
        getNumCols  m = (+) 1 $ snd $ snd $ bounds m
        myChunksOf :: Int -> [a] -> [[a]]
        myChunksOf _ [] = []
        myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))

elementFromMatrix :: (Ord e) => ((Int, Int), e) -> (MatrixElement e)
elementFromMatrix em = MatrixElement em

matrixFromList xss =
    matrix
    where
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)



