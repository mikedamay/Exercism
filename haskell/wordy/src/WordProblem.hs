module WordProblem (answer) where

import Data.List (words)
import Text.Read (readMaybe)

data ParsedAs = Operand Integer | BinOp (Integer -> Integer -> Integer) | Err

instance Show (ParsedAs) where
    show (Operand x) = show x
    show Err = "Err"
    show _ = "BinOp"

answer :: String -> Maybe Integer
answer problem = compute $ map parseToken cleaned
    where cleaned = filter (/= "by") $ drop 2 $ words $ trimEnd problem

compute :: [ParsedAs] -> Maybe Integer
compute [] = Nothing
compute ((Operand x):[]) = Just x
compute (Err:[]) = Nothing
compute (_:[]) = Nothing
compute (_:_:[]) = Nothing
-- compute ((op1):(operator):(op2):xs) = compute ((binop (Operand 13) (BinOp (+)) (Operand 12)):xs)
compute ((op1):(operator):(op2):xs) = compute ((binop op1 operator op2):xs)

binop :: ParsedAs -> ParsedAs -> ParsedAs -> ParsedAs
binop (Operand op1) (BinOp operator) (Operand op2) = Operand (operator op1 op2)
binop _ _ _ = Err

parseToken :: String -> ParsedAs
parseToken x
    | x == "plus" = BinOp (+)
    | x == "divided" = BinOp div
    | x == "multiplied" = BinOp (*)
    | x == "minus" = BinOp (-)
    | ((readMaybe x) :: Maybe Int) == Nothing = Err
    | ((readMaybe x) :: Maybe Int) /= Nothing = Operand (read x)
    | otherwise = Err


trimEnd :: String -> String
trimEnd = foldr (\x acc -> if null acc && x == '?' then [] else x:acc) []
