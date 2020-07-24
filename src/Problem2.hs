module Problem2 where

problem :: [Int]
problem =  1 : 2 : zipWith (+) problem (tail problem)
