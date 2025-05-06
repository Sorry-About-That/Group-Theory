module Util where


elemIndex :: Eq a => a -> [a] -> Int
x `elemIndex` (y:ys)
  | x == y    = 0
  | otherwise = (x `elemIndex` ys) + 1