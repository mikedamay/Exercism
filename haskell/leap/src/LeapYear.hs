module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
    | year <= 0 = False
    | year `divisibleBy` 4 = not (year `divisibleBy` 100) || year `divisibleBy` 400
    | otherwise = False
    where divisibleBy x y = x `rem` y == 0