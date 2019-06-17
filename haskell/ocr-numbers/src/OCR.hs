module OCR (convert) where

import Data.List (transpose, intersperse)

convert :: String -> String
convert = toNumber . toNumberGroup . toDigits . toSlices . lines

strToDigit :: [String] -> Char
strToDigit [" _ ","| |","|_|","   "] = '0'
strToDigit ["   ","  |","  |","   "] = '1'
strToDigit [" _ "," _|","|_ ","   "] = '2'
strToDigit [" _ "," _|"," _|","   "] = '3'
strToDigit ["   ","|_|","  |","   "] = '4'
strToDigit [" _ ","|_ "," _|","   "] = '5'
strToDigit [" _ ","|_ ","|_|","   "] = '6'
strToDigit [" _ ","  |","  |","   "] = '7'
strToDigit [" _ ","|_|","|_|","   "] = '8'
strToDigit [" _ ","|_|"," _|","   "] = '9'
strToDigit _                         = '?'


toNumber :: [String] -> String
toNumber = concat . (intersperse ",")

toNumberGroup :: [[[String]]] -> [String]
toNumberGroup xs = map (map strToDigit) xs

toDigits :: [[[String]]] -> [[[String]]]
toDigits xs = map transpose xs

toSlices :: [String] -> [[[String]]]
toSlices xs = map (map (myChunksOf 3)) $ myChunksOf 4 xs

myChunksOf :: Int -> [a] -> [[a]]
myChunksOf _ [] = []
myChunksOf n xs = (take n xs) : (myChunksOf n $ drop n xs)







