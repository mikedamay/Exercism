module RunLength (decode, encode) where

import Data.Char (isDigit)


decode :: String -> String
decode encodedText = concat $ map buildString $ foldr rldecode [] encodedText

encode :: String -> String
encode text = concat $ map encodingToText $ foldr rle [] text
  where
    rle c [] = [(c, 1)]
    rle c (all@(ac, n):xs)
        | ac == c = (ac, n+1) : xs
        | otherwise = (c, 1) : all : xs
    encodingToText (c, 1) = [c]
    encodingToText (c, n) = (show n) ++ [c]

rlencode :: Char -> (Char, Int, [Char]) -> (Char, Int, [Char])
rlencode c ('0', 0, []) = (c, 1, [c])
rlencode c (ca, n, l)
    | ca == c = (c, n+1, l)
    | otherwise = (c, 1, c:(if n > 1 then (show n) ++ l else l))

rlencode2 :: [(Char, Int)] -> String -> [(Char, Int)]
rlencode2 xs [] = xs
rlencode2 [] (c:cs) = rlencode2 [(c, 1)] cs
rlencode2 (all@(ac, n):xs) (c:cs)
    | ac == c = rlencode2 ((ac, n+1):xs) cs
    | otherwise = rlencode2 ((c, 1):all:xs) cs

rldecode :: Char -> [([Char], Char)] -> [([Char], Char)]
rldecode c [] = [("", c)]
rldecode c ((ct, ch):xs) | isDigit c = (c:ct, ch):xs
rldecode c xs = ("", c):xs

buildString :: ([Char], Char) -> [Char]
buildString (num, ch) = ch : if length num > 0 then (replicate ((read num) - 1) ch) else ""

