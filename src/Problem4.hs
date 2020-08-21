module Problem4 where

solution :: Int -> (Int, Int, Int) 
solution digit = 
    let startNumber = maxNumberDigit digit in
        f startNumber startNumber 
            where f multiplying multiplied = 
                        let 
                            g x y 
                             | isPalindrome (digs currentNum) = (currentNum, x, y) 
                             | x > 1 = g (x-1) y 
                             | otherwise = f (multiplying) (y-1) 
                             where currentNum = x*y 
                        in g multiplying multiplied

digs :: Int -> [Int]
digs number = f number 
    where f n
            | n < 10 = [n]  
            | otherwise = f (n `div` 10) ++ [(n `mod` 10)] 

isPalindrome :: [Int] -> Bool
isPalindrome numberSeq = numberSeq == (reverse numberSeq)

maxNumberDigit :: Int -> Int 
maxNumberDigit digit = (10^digit)-1  



