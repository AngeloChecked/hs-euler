module Main where

import Control.Monad

main :: IO ()
main = do 
    
    print [1,1,2]
    x <- return "ciao"
    print x

test :: [Int]
test = do 
    x <- [1::Int,2,3]
    pure x
