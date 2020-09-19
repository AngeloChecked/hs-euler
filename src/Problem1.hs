module Problem1 where

import  Control.Monad
import qualified Data.Set as Set

solution1 n = sum $ do 
  x <- [1..n]
  guard $ any ((== 0) . mod x) [3,5]
  pure x

solution2 n = sum [ x | x <- [1..n], any ((== 0) . mod x) [3,5] ] 

solution3 :: Int -> Int
solution3 n = 
    sum (Set.union (Set.fromList [3,6..n]) (Set.fromList [5,10..n]))


