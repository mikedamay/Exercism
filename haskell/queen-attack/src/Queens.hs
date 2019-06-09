module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString whiteQueen blackQueen = unlines $ board whiteQueen blackQueen

board :: Maybe (Int, Int) -> Maybe (Int, Int) -> [String]
board Nothing Nothing = emptyBoard
board (Just queen) Nothing = pubQueenOnEmptyBoard queen 'W'
board Nothing (Just queen) = pubQueenOnEmptyBoard queen 'B'
board (Just whiteQueen) (Just blackQueen) = putQueenOnBoard (pubQueenOnEmptyBoard blackQueen 'B') whiteQueen 'W'

putQueenOnBoard :: [String] -> (Int, Int) -> Char -> [String]
putQueenOnBoard bboard (r, c) queenChar = map (substituteInRow (r, c) queenChar) (zip bboard [0..])

pubQueenOnEmptyBoard :: (Int, Int) -> Char -> [String]
pubQueenOnEmptyBoard = putQueenOnBoard emptyBoard

emptyBoard :: [String]
emptyBoard = replicate 8 row
    where row = "_ _ _ _ _ _ _ _"

substituteInRow :: (Int, Int) -> Char -> (String, Int) -> String
substituteInRow (r, c) queenChar (row, rowNum)
    | r == rowNum = zipWith (substituteCol (c*2) queenChar) row [0..]
    | otherwise = row

substituteCol :: Int -> Char -> Char -> Int -> Char
substituteCol col queenChar ch c
    | col == c = queenChar
    | otherwise = ch

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ra, ca) (rb, cb)
    | ra == rb = True
    | ca == cb = True
    | (abs (ra - rb)) == (abs (ca - cb)) = True
    | otherwise = False
