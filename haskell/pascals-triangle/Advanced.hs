module Advanced where

rows :: Int -> [[Integer]]
rows n = take n pascal where
  pascal = [1] : map next pascal
  next r = zipWith (+) (0:r) $ r++[0]

-- this bit of recursion bemuses me
-- pascal = [1] : map next pascal

