module RunLength (decode, encode) where

import Data.Char (isDigit)


decode :: String -> String
decode encodedText = concat $ map buildString $ foldr rldecode [] encodedText

encode :: String -> String
encode text = if count > 1 then (show count) ++ encoded else encoded
  where
    (_, count, encoded) = foldr rlencode ('0', 0, []) text

rlencode :: Char -> (Char, Int, [Char]) -> (Char, Int, [Char])
rlencode c ('0', 0, []) = (c, 1, [c])
rlencode c (ca, n, l)
    | ca == c = (c, n+1, l)
    | otherwise = (c, 1, c:(if n > 1 then (show n) ++ l else l))

rldecode :: Char -> [([Char], Char)] -> [([Char], Char)]
rldecode c [] = [("", c)]
rldecode c ((ct, ch):xs) | isDigit c = (c:ct, ch):xs
rldecode c xs = ("", c):xs

buildString :: ([Char], Char) -> [Char]
buildString (num, ch) = ch : if length num > 0 then (replicate ((read num) - 1) ch) else ""

