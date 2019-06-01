module School (School, add, empty, grade, sorted) where

import Data.List (sort, sortBy)
import qualified Data.Map as Map

data Entry = Entry {gradenum :: Int, student :: String} deriving (Eq, Ord, Show)

type School = Map.Map Int [String]

add :: Int -> String -> School -> School
add gn st school = Map.insertWith combine gn [st] school

combine :: [String] -> [String] -> [String]
combine st sts = sort (st ++ sts)

empty :: School
empty = Map.empty

grade :: Int -> School -> [String]
grade gn school
    | Map.lookup gn school == Nothing = []
    | otherwise = sort (snd (head (filter (\x -> (fst x) == gn) (Map.toList school))))

sorted :: School -> [(Int, [String])]
sorted school = sortBy (\x y -> compare (fst x) (fst y)) [x | x <- (Map.toList school)]


