module Problem1 where

problem :: Int -> Int
problem n = sum [ x | x <- [1..n], (mod x 3) == 0 || (mod x 5) == 0] 
