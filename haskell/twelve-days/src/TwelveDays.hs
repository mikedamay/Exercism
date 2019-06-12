module TwelveDays (recite) where

import Data.List (unwords, intersperse)

days :: [String]
days = [
    ""
    ,"first"
    ,"second"
    ,"third"
    ,"fourth"
    ,"fifth"
    ,"sixth"
    ,"seventh"
    ,"eigth"
    ,"ninth"
    ,"tenth"
    ,"eleventh"
    ,"twelfth"
    ]

onThe = "On the " :: String

dayOf = " day of Christmas my true love gave to me:"

inventoryStr :: String
inventoryStr = "twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, a Partridge in a Pear Tree"

recite :: Int -> Int -> [String]
recite start stop = map reciteForDay [start .. stop]

reciteForDay :: Int -> String
reciteForDay d = onThe ++ (show (days !! d)) ++ dayOf ++ (getPresentsForDay d)

inventory :: [String]
inventory = toList inventoryStr

toList :: String -> [String]
toList [] = []
toList xs = chunk : (toList $ (drop $ length chunk + 1) xs)
    where chunk = takeWhile (/= ',') xs


getText :: Int -> Int -> [String]
getText start stop = [preamble]
    where preamble = onThe ++ (days !! stop) ++ dayOf

getPresentsForDay :: Int -> String
getPresentsForDay d = concat $ intersperse "," $ reverse $ addAnd $ reverse $ drop (12 - d) inventory


addAnd :: [String]  -> [String]
addAnd [] = []
addAnd (x:[]) = [x]
addAnd (x:xs) = ((" and" ++ x) : xs)
