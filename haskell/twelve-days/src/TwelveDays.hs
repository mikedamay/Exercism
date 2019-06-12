module TwelveDays (recite) where

import Data.List (intersperse)

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
    ,"eighth"
    ,"ninth"
    ,"tenth"
    ,"eleventh"
    ,"twelfth"
    ]

onThe :: String
onThe = "On the "

dayOf :: String
dayOf = " day of Christmas my true love gave to me:"

inventoryStr :: String
inventoryStr = " twelve Drummers Drumming, eleven Pipers Piping, ten Lords-a-Leaping, nine Ladies Dancing, eight Maids-a-Milking, seven Swans-a-Swimming, six Geese-a-Laying, five Gold Rings, four Calling Birds, three French Hens, two Turtle Doves, a Partridge in a Pear Tree"

recite :: Int -> Int -> [String]
recite start stop = map reciteForDay [start .. stop]

reciteForDay :: Int -> String
reciteForDay d = onThe ++ (days !! d) ++ dayOf ++ (getPresentsForDay d) ++ "."

inventory :: [String]
inventory = toList inventoryStr

toList :: String -> [String]
toList [] = []
toList xs = chunk : (toList $ (drop $ length chunk + 1) xs)
    where chunk = takeWhile (/= ',') xs

getPresentsForDay :: Int -> String
getPresentsForDay d = concat $ intersperse "," $ reverse $ addAnd $ reverse $ drop (12 - d) inventory

addAnd :: [String]  -> [String]
addAnd [] = []
addAnd (x:[]) = [x]
addAnd (x:xs) = ((" and" ++ x) : xs)
