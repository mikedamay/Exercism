module RunLength (decode, encode) where

import Data.Char (isDigit)


decode :: String -> String
decode encodedText = concat $ map buildString $ foldr rld [] encodedText
  where
    rld c [] = [("", c)]
    rld c ((ct, ch):xs) | isDigit c = (c:ct, ch):xs
    rld c xs = ("", c):xs
    buildString (num, ch)
        | length num > 0 = replicate (read num) ch
        | otherwise = [ch]

encode :: String -> String
encode text = concat $ map encodingToText $ foldr rle [] text
  where
    rle c [] = [(c, 1)]
    rle c (acn@(ac, n):xs)
        | ac == c = (ac, n+1) : xs
        | otherwise = (c, 1) : acn : xs
    encodingToText :: (Char, Int) -> [Char]
    encodingToText (c, 1) = [c]
    encodingToText (c, n) = (show n) ++ [c]
