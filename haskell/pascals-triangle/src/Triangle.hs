module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows x = take x ([1] : iterate ((1:) . buildTriangle) [1, 1])

buildTriangle :: [Integer] -> [Integer]
buildTriangle [] = []       -- avoid warning
buildTriangle (_:[]) = [1]
buildTriangle row@(x:y:_) = x+y : buildTriangle (tail row)
