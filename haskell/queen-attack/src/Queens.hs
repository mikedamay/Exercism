module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = unlines emptyBoard
boardString (Just (r, c)) Nothing = unlines $ map (substituteInRow (r, c) 'W') (zip emptyBoard [0..])
boardString Nothing (Just (r, c)) = unlines $ map (substituteInRow (r, c) 'B') (zip emptyBoard [0..])
boardString (Just (wr, wc)) (Just (br, bc)) = unlines $ map (substituteInRow (br, bc) 'B') $ zip (map (substituteInRow (wr, wc) 'W') (zip emptyBoard [0..])) [0..]

emptyBoard :: [String]
emptyBoard = replicate 8 row
    where row = "_ _ _ _ _ _ _ _"

substituteInRow :: (Int, Int) -> Char -> (String, Int) -> String
substituteInRow (r, c) queen (row, rowNum)
    | r == rowNum = zipWith (substituteCol (c*2) queen) row [0..]
    | otherwise = row

substituteCol :: Int -> Char -> Char -> Int -> Char
substituteCol col queen ch c
    | col == c = queen
    | otherwise = ch

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ra, ca) (rb, cb)
    | ra == rb = True
    | ca == cb = True
    | (abs (ra - rb)) == (abs (ca - cb)) = True
    | otherwise = False
