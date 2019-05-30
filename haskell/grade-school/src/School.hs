module School (School, add, empty, grade, sorted) where

import Data.List (nub, sort, sortBy)
import qualified Data.Map as Map

data Entry = Entry {gradenum :: Int, student :: String} deriving (Eq, Ord, Show)

type School = Map.Map Int [String]

add :: Int -> String -> School -> School
-- add gn st school = Map.fromList ((gn,st) : [(1, "abc")])
add gn st school = Map.fromList (merge (gn,st) (Map.toList school))

merge :: (Int, String) -> [(Int, [String])] -> [(Int, [String])]
merge y [] = [((fst y), [(snd y)])] :: [(Int, [String])]
merge y (x:xs)
    | (fst y) == (fst x) = ((fst x), ((snd y):(snd x))) : (merge y xs)
    | otherwise = (x:(merge y xs))

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
-- grade gn school =  sort [snd x | x <- (Map.toList school), (fst x) == gn]
grade gn school = sort (snd (head (filter (\x -> (fst x) == gn) (Map.toList school))))

sorted :: School -> [(Int, [String])]
sorted school = sorted_school
    where sorted_school = sortBy (\x y -> compare (fst x) (fst y)) [x | x <- (Map.toList school)]


