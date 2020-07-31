module Problem1Spec where

import Test.Hspec
import Problem1

spec :: Spec
spec = 
    describe "Problem 1" $

        describe "should return the sum of all natural number that are multiples of 3 or 5" $ do

            it "solution1" $ do
                solution1 9 `shouldBe` 23 
                solution1 10 `shouldBe` 33
                solution1 11 `shouldBe` 33
                solution1 12 `shouldBe` 45 
                solution1 13 `shouldBe` 45 
                solution1 14 `shouldBe` 45 
                solution1 15 `shouldBe` 60 
                solution1 999 `shouldBe` 233168 

            it "solution2" $ do
                solution2 9 `shouldBe` 23 
                solution2 10 `shouldBe` 33
                solution2 11 `shouldBe` 33
                solution2 12 `shouldBe` 45 
                solution2 13 `shouldBe` 45 
                solution2 14 `shouldBe` 45 
                solution2 15 `shouldBe` 60 
                solution2 999 `shouldBe` 233168 
             
            it "solution3" $ do
                solution3 9 `shouldBe` 23 
                solution3 10 `shouldBe` 33
                solution3 11 `shouldBe` 33
                solution3 12 `shouldBe` 45 
                solution3 13 `shouldBe` 45 
                solution3 14 `shouldBe` 45 
                solution3 15 `shouldBe` 60 
                solution3 999 `shouldBe` 233168 
