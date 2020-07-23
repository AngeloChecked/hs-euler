module Problem1 where

import  Control.Monad

problem n = sum $ do 
  x <- [1..n]
  guard $ any ((== 0) . mod x) [3,5]
  pure x







-- problem n = sum [ x | x <- [1..n], (mod x 3) == 0 || (mod x 5) == 0] 
