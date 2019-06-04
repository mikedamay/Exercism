module Minesweeper (annotate, addCoordinates, addCoordsToLine) where

annotate :: [String] -> [String]
annotate board = []

addCoordinates :: [String] -> [[(Int, Int, Int)]]
addCoordinates [] = []
addCoordinates b = map addCoordsToLine $ zip b [0 :: Int ..]

addCoordsToLine :: ([Char], Int) -> [(Int, Int, Int)]
addCoordsToLine (line,row) = map (\x -> (row, snd x, if (fst x) == '*' then -1 else 0) ) $ zip line [0..]
