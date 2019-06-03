module Diamond (diamond, generateLine, numCols, letterIndex) where

diamond :: Char -> Maybe [String]
diamond ch = Nothing

generateLine :: Char -> Char -> String
generateLine targetCh = generateLine' targetCh
generateLine' targetCh ch = padLeft ++ [ch] ++ padMid ++ secondCh ++ padRight
    where padLeft = take (((numCols targetCh) `div` 2) - offset) (repeat ' ')
          padMid = take ((numCols targetCh) - ((length padLeft) * 2) - 2) (repeat ' ')
          padRight = padLeft
          offset = letterIndex ch
          secondCh = if ch == 'A' then [] else [ch]

numCols :: Char -> Int
numCols ch = (+) 1 $ (*) 2 $ letterIndex ch

letterIndex :: Char -> Int
letterIndex ch = (fromEnum ch) - (fromEnum 'A')
