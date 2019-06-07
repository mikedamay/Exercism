module SecretHandshake (handshake) where

import Data.Bits

handshake :: Int -> [String]
handshake n = forwardOrBack n $ filter (/= "") $ map ($ n) secretSigns

secretSigns :: [(Int -> String)]
secretSigns = [0x1 `codeToGesture` "wink", 0x2 `codeToGesture` "double blink", 0x4 `codeToGesture` "close your eyes", 0x8 `codeToGesture` "jump"]

forwardOrBack :: Int -> ([a] -> [a])
forwardOrBack n
    | n .&. 0x10 == 0x10 = reverse
    | otherwise = id

codeToGesture :: Int -> String -> Int -> String
codeToGesture code gesture handshake'
    | code .&. handshake' == code = gesture
    | otherwise = ""