module Tar where

import Data.List
import Data.Function

tar :: [[Int]]
tar = f [] 
    where f s = next : (f next)   
                    where next = nextT s

tar2 :: [[Int]]
tar2 = iterate nextT [1] 
    
nextT :: [Int] -> [Int]
nextT toSum = 1 : f toSum 
        where
              f [] = [] 
              f (x:[]) = [1]
              f (x:yxs@(y:xs)) = x+y : f yxs 

-- paolino
nextT2 :: [Int] -> [Int]
nextT2 = (1:). (++ [1]) . (\xs -> zipWith (+) xs (tail xs))

tart :: [[Int]]
tart = fix $ ([] :) . fmap ((1:) . (++ [1]) . (zipWith (+) <*> tail))


