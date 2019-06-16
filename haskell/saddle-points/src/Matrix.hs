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

data MatrixElement = MatrixElement ((Int, Int), Int) deriving (Eq)

saddlePoints :: Array (Int, Int) Int -> [(Int, Int)]
saddlePoints matrix = prepare $ listFromMatrix matrix

prepare :: [[MatrixElement]] -> [(Int, Int)]
prepare xs = map (\(MatrixElement ((r, c), v)) -> (r, c)) $ concat $ getSaddlePoints [(x, y) | x <- getMaxElements xs, y <- getMinElements $ transpose xs ]

getSaddlePoints :: [([MatrixElement],[MatrixElement])] -> [([MatrixElement])]
getSaddlePoints = map getSaddlePoint
    where
        getSaddlePoint :: ([MatrixElement], [MatrixElement]) -> ([MatrixElement])
        getSaddlePoint (rows, cols) = [ r | r <- rows, c <- cols, r == c]

getMaxElements :: [[MatrixElement]] -> [[MatrixElement]]
getMaxElements = map (foldl findMax [])
    where findMax = saddlePoint (>)

getMinElements :: [[MatrixElement]] -> [[MatrixElement]]
getMinElements = map (foldl findMin [])
    where findMin = saddlePoint (<)

saddlePoint :: (Int -> Int -> Bool) -> [MatrixElement] -> MatrixElement -> [MatrixElement]
saddlePoint fc [] new = [new]
saddlePoint fc acc@((MatrixElement ((racc, cacc), vacc)):xs) new@(MatrixElement((r, c), v))
    | fc v vacc = [MatrixElement ((r, c), v)]
    | v == vacc = new:acc
    | otherwise = acc

listFromMatrix :: (Array (Int, Int) Int) -> [[MatrixElement]]
listFromMatrix m = myChunksOf (getNumCols m) $ map elementFromMatrix $ assocs m
    where
        getNumCols :: (Array (Int, Int) Int) -> Int
        getNumCols  m = (+) 1 $ snd $ snd $ bounds m
        myChunksOf :: Int -> [a] -> [[a]]
        myChunksOf _ [] = []
        myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))

elementFromMatrix :: ((Int, Int), Int) -> MatrixElement
elementFromMatrix em = MatrixElement em

matrixFromList xss =
    matrix
    where
        rows      = length xss
        columns   = length $ head xss
        matrix    = listArray ((0, 0), (rows - 1, columns - 1)) (concat xss)



