module School (School, add, empty, grade, sorted, merge) where

import Data.List (sort, sortBy)
import qualified Data.Map as Map

data Entry = Entry {gradenum :: Int, student :: String} deriving (Eq, Ord, Show)

type School = Map.Map Int [String]

add :: Int -> String -> School -> School
add gn st school
    | school_list `contains` gn =  Map.fromList (merge (gn,st) school_list)
    | otherwise = Map.fromList ((gn,[st]) : school_list)
    where school_list = Map.toList school

contains :: [(Int,[String])] -> Int-> Bool
contains [] _ = False
contains (x:xs) gn = (gn == (fst x)) || (contains xs gn)

merge :: (Int, String) -> [(Int, [String])] -> [(Int, [String])]
merge _ [] = []
merge y (x:xs)
    | (fst y) == (fst x) = [((fst x), sort ((snd y):(snd x)))] ++ xs
    | otherwise = (x:(merge y xs))

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gn school
    | Map.lookup gn school == Nothing = []
    | otherwise = sort (snd (head (filter (\x -> (fst x) == gn) (Map.toList school))))

sorted :: School -> [(Int, [String])]
sorted school = sorted_school
    where sorted_school = sortBy (\x y -> compare (fst x) (fst y)) [x | x <- (Map.toList school)]


