module Problem3 where

solution1 :: Int -> [Int] 
solution1 n = f 2 
    where f m 
            | m == n = []
            | mod n m == 0 = if isPrime m then m : f (m+1)  else f (m+1)              
            | otherwise = f (m+1)

isPrime :: (Eq a, Integral a) => a -> Bool
isPrime x = g (x-1)
         where g y 
                 | y == 1 = True 
                 | mod y 2 == 0 = g (y-1)
                 | mod x y == 0 = False | otherwise = g (y-1)

solution2 :: Int -> Int
solution2 n = f (n-1)
        where f m 
                | m == 1 = 1
                | mod n m == 0 && isPrime m = m 
                | otherwise = f (m-1) 

solution3 n = f n 2
     where f m p 
             |  p*p > m = if m > p then m else p 
             |  mod m p == 0 = 
                 let newnumm = (m/p) in 
                    f newnumm p 
             | otherwise = f m (p+1) 
