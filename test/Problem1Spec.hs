module Problem1Spec where

import Test.Hspec
import Problem1

spec :: Spec
spec = 
    describe "Problem 1" $

        it "should return the sum of all natural number that are multiples of 3 or 5" $ do
            problem 9 `shouldBe` 23 
            problem 10 `shouldBe` 33
            problem 11 `shouldBe` 33
            problem 12 `shouldBe` 45 
            problem 13 `shouldBe` 45 
            problem 14 `shouldBe` 45 
            problem 15 `shouldBe` 60 
            problem 999 `shouldBe` 233168 
