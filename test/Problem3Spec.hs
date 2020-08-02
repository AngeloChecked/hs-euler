module Problem3Spec where

import Test.Hspec
import Problem3

spec :: Spec
spec = 
        describe "problem3" $
            describe "found prime factor numbers" $ do
                it "solution1" $ do
                    solution1 13195 `shouldBe` [5,7,13,29]
                it "solution2" $ do
                    solution2 13195 `shouldBe` 29
                it "solution3" $ do
                    solution3 13195 `shouldBe` 29
                    solution3 600851475143 `shouldBe` 6857 

instance Fractional Integer where
    (/) = div 
    fromRational = undefined















