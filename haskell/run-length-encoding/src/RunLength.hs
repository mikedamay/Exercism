module RunLength (decode, encode) where



decode :: String -> String
decode encodedText = error "You need to implement this function."

encode :: String -> String
encode text = (show count) ++ encoded
  where
    (_, count, encoded) = foldr rlencode ('0', 0, []) text

rlencode :: Char -> (Char, Int, [Char]) -> (Char, Int, [Char])
rlencode c ('0', 0, []) = (c, 1, [c])
rlencode c (ca, n, l)
    | ca == c = (c, n+1, c:l)
    | otherwise = (c, 1, (show n) ++ l)

