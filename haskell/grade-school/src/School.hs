module School (School, add, empty, grade, sorted, mysort) where

import Data.List (nub, sortBy)

data Entry = Entry {gradenum :: Int, student :: String} deriving (Eq, Ord, Show)

type School = [Entry]

add :: Int -> String -> School -> School
add gn st school = Entry {gradenum = gn, student = st} : school

empty :: School
empty = []

grade :: Int -> School -> [String]
grade gn school = [(student x) | x <- (mysort [x | x <- school, (gradenum x) == gn])]

sorted :: School -> [(Int, [String])]
sorted school = [(g, [(student e) | e <- sorted_school, (gradenum e) == g])  | g <- grades]
    where sorted_school = sortBy (\x y -> compare ((gradenum x), (student x)) ((gradenum y), (student y))) [x | x <- school]
          grades = nub [(gradenum x) | x <- sorted_school]

