module Clock (addDelta, fromHourMin, toString, normalise) where

data Clock = Clock Int Int

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock hour min

toString :: Clock -> String
toString clock = showclock clock

showclock (Clock h m) = (Prelude.show h) ++ ":" ++ (Prelude.show m)

normalise h m = let mins = h * 60 + m in mins `mmod` 1440


mmod x y = (((x `mod` y) + y) `mod` y)

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock ch cm) = Clock 0 0
