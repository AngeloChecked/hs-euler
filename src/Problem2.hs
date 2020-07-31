module Problem2 where

solution1 :: [Int]
solution1 =  1 : 2 : zipWith (+) solution1 (tail solution1)

solution2 :: Int -> [Int] 
solution2 0 = []
solution2 n = solution2 (n-1) ++ [fib n]
    where fib 1 = 1
          fib 2 = 2 
          fib n = fib (n-1) + fib (n-2) 

