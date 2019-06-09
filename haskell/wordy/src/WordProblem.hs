module WordProblem (answer) where

import Data.List (words)
import Data.Char (isDigit)
import Text.Read (readMaybe)

data ParsedAs = Operand Integer | BinOp (Integer -> Integer -> Integer) | Separator | Err

answer :: String -> Maybe Integer
answer problem = compute $ map parseToken cleaned
    where cleaned = drop 2 $ words $ trimEnd problem

compute :: [ParsedAs] -> Maybe Integer
compute [] = Just 0
compute ((Operand x):[]) = Just x
compute (Err:[]) = Nothing
compute (_:[]) = Nothing
compute (op1:operator:[]) = Nothing
compute (op1:operator:op2:xs) = compute ((binop op1 operator op2):xs)

binop :: ParsedAs -> ParsedAs -> ParsedAs -> ParsedAs
binop (Operand op1) (BinOp operator) (Operand op2) = Operand (operator op1 op2)

parseToken :: String -> ParsedAs
parseToken x
    | ((readMaybe x) :: Maybe Int) == Nothing = Err
    | ((readMaybe x) :: Maybe Int) /= Nothing = Operand (read x)
    | x == "plus" = BinOp (+)
    | x == "divided" = BinOp div
    | x == "multiplied" = BinOp (-)
    | x == "minus" = BinOp (*)
    | x == "by" = Separator
    | otherwise = Err



trimEnd :: String -> String
trimEnd = foldr (\x acc -> if null acc && not (isDigit x) then [] else x:acc) []
