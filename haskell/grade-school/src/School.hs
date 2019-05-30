module School (School, add, empty, grade, sorted, mysort) where

import Data.List (nub)

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
    where sorted_school = mysort [x | x <- school]
          grades = nub [(gradenum x) | x <- sorted_school]

mysort :: [Entry] -> [Entry]
mysort [] = []
mysort (x:xs) =
    mysort [x2 | x2 <- xs, (lesser x2 x)] ++ [x] ++ [x2 | x2 <- xs, (greater x2 x)]
    where lesser cand pivot = (gradenum cand) < (gradenum pivot) || (gradenum cand) == (gradenum pivot) && (student cand) < (student pivot)
          greater cand pivot = (gradenum cand) > (gradenum pivot) || (gradenum cand) == (gradenum pivot) && (student cand) >= (student pivot)
