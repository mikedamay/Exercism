module Clock (addDelta, fromHourMin, toString, normalise, norm, mmod) where

import Text.Printf

data Clock = Clock Int Int deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = norm hour min 0

toString :: Clock -> String
toString clock = showclock clock

showclock (Clock h m) = printf "%02d:%02d" h m

norm h m mm = let mins = normalise h m mm in Clock (mins `div` 60) (mins `mod` 60)

normalise h m mm = let mins = h * 60 + m + mm in mins `mmod` 1440

mmod x y = (((x `mod` y) + y) `mod` y)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock ch cm) = (norm ch cm (hour * 60 + min))

{-
instance Eq Clock where
Clock h m == Clock hh mm = h Prelude.== hh && m Prelude.== mm-}
