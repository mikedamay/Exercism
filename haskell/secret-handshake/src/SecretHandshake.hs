module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = forwardOrBack n $ filter (/="") $ map ($ n) signs

signs :: [(Int -> String)]
signs = [0x1 `gives` "wink", 0x2 `gives` "double blink", 0x4 `gives` "close your eyes", 0x8 `gives` "jump"]

forwardOrBack :: Int -> ([a] -> [a])
forwardOrBack n
    | n .&. 0x10 == 0x10 = reverse
    | otherwise = id

gives :: Int -> String -> Int -> String
gives val sign handshake'
    | val .&. handshake' == val = sign
    | otherwise = ""