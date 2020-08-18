module Problem4Spec where

import Test.Hspec
import Problem4

spec :: Spec
spec = 
    describe "problem 4" $ do
        it "number array digit" $ 
            digs 12345 `shouldBe` [1,2,3,4,5]
        it "is palindrome" $
            isPalindrome [1,1,2,1,1] `shouldBe` True
        it "get max number with digit" $ do
            maxNumberDigit 1 `shouldBe` 9 
            maxNumberDigit 2 `shouldBe` 99 
            maxNumberDigit 3 `shouldBe` 999 
            maxNumberDigit 4 `shouldBe` 9999 
            maxNumberDigit 5 `shouldBe` 99999 
            maxNumberDigit 9 `shouldBe` 999999999 
        describe "largest palindome number made from the product of number digit" $ do
            it "with 2 digit" $ 
               solution 2 `shouldBe` (9009, 91, 99) 
            it "with 3 digit" $
               solution 3 `shouldBe`  (90909, 91, 999) 

