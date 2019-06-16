module Matrix (saddlePoints) where

import Data.Array (Array, bounds, assocs)
import Data.List (transpose)

data MatrixElement e = MatrixElement ((Int, Int), e) deriving (Eq)

saddlePoints :: (Ord e, Eq e) => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = prepare $ listFromMatrix matrix

prepare :: (Ord e, Eq e) => [[MatrixElement e]] -> [(Int, Int)]
prepare xs = map (\(MatrixElement ((r, c), _)) -> (r, c)) $ concat $ getSaddlePoints [(x, y) | x <- getMaxElements xs, y <- getMinElements $ transpose xs ]

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
saddlePoint _ [] new = [new]
saddlePoint fc acc@((MatrixElement (_, vacc):_)) new@(MatrixElement((r, c), v))
    | fc v vacc = [MatrixElement ((r, c), v)]
    | v == vacc = new:acc
    | otherwise = acc

listFromMatrix :: (Ord e) => (Array (Int, Int) e) -> [[MatrixElement e]]
listFromMatrix m = myChunksOf numCols $ map elementFromMatrix $ assocs m
    where
        numCols :: Int
        numCols  = (+) 1 $ snd $ snd $ bounds m
        myChunksOf :: Int -> [a] -> [[a]]
        myChunksOf _ [] = []
        myChunksOf n xs = (take n xs) : (myChunksOf n (drop n xs))

elementFromMatrix :: (Ord e) => ((Int, Int), e) -> (MatrixElement e)
elementFromMatrix em = MatrixElement em
