{-# LANGUAGE TupleSections, ViewPatterns #-}
module Problem4 where

import Data.Function
import Data.List 
import Data.Ord
import qualified Data.Map as MAP 
import Control.Arrow 

solution :: Int -> (Int, Int, Int) 
solution digit = 
    let startNumber = maxNumberDigit digit in
        maximum (f startNumber startNumber) 
            where 
                minNumDigit = (maxNumberDigit (digit-1))+1
                f multiplying multiplied = 
                        let 
                            g x y 
                             | y <= minNumDigit = []
                             | isPalindrome (digs currentNum) = (currentNum, x, y) : g (x-1) y 
                             | x > minNumDigit = g (x-1) y 
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

isPalindrome2 n = reverseN2 n == n 

reverseN :: Int -> Int 
reverseN b = 
    let digitNumber = floor (logBase 10 (fromIntegral b))+1 in
    f b (digitNumber-1)
        where f n 0 = n 
              f n digit = 
                  (n `mod` 10) * 10^digit + f (n `div` 10) (digit-1) 

reverseN2 :: Integral b => b -> b
reverseN2 x = snd $ fix $ \(fst -> q) -> r x $ 10 ^ q
  where
    r 0 n = (-1, 0)
    r x n =
      let (y, z) = divMod x 10
       in succ *** (z * n +) $ r y (div n 10)

maxNumberDigit :: Int -> Int 
maxNumberDigit digit = (10^digit)-1  


solution2 = maximum [(i*j,i,j) | i <- [x | x <- reverse [100..999], mod x 11 ==0], j <- reverse [100..999], i <= j, isPalindrome $ i * j]
  where isPalindrome n = (read . reverse . show) n == n


selectf :: (a -> Maybe (b, a)) -> [a] -> [(b, [a])]
selectf h list = f list id 
    where f [] _ = []
          f (x:xs) g = case (h x) of
                         (Just (b,a)) -> (g . (a:) <$> (b, xs)) : f xs (g . (x:))    
                         Nothing -> f xs (g . (x:))    

step :: (a -> a -> Ordering) -> [[a]] -> Maybe (a, [[a]])
step h ls = case (selectf uncons ls) of
                   [] -> Nothing
                   ls -> Just $ maximumBy (h `on` fst) ls  

prods m = inside <$> down n
  where
    n = pred $ z * 10
    z = 10 ^ pred m
    inside r = (r,) <$> down r
    down x = [x, x - 1 .. z]

driveStep :: (a -> a -> Ordering) -> [[a]] -> [a]
driveStep g = unfoldr (step g)   

enumerate n = driveStep (comparing $ uncurry (*)) (prods n)

paolinoSolution = 
    filter (\(_,_,n) -> reverseN n == n) $ take 6000 $ (\(x,y) -> (x, y , x * y)) <$> enumerate 3


type Product = (Int, Int)
type Queue = MAP.Map Int [Product]
 
selectMap :: [(Int,Int)] -> Queue 
selectMap chunk = MAP.fromList (f chunk id)
        where f [] _ = []
              f ((a,b):xs) g = (((a,b):) . g <$> (a*b,xs)) : f xs (g . ((a,b):)) 

stepMap :: Queue -> [(Int, Product)]
stepMap m = case MAP.lookupMax m of
  Nothing -> []
  Just (x, yh : yt) ->
    let p = if isPalindrome2 x then ((x, yh) :) else id
        f = case yt of
          (a, b) : _ -> MAP.insert (a * b) yt
          [] -> id
     in p $ stepMap (f . MAP.delete x $ m)

