module Problem2Spec where
    
import Test.Hspec
import Problem2

spec :: Spec
spec = 
    describe "problem 2" $ do
        describe "fibonacci sequence starting with 1 end 2" $ do
           it "solution1" $ 
               take 10 solution1 `shouldBe` [1,2,3,5,8,13,21,34,55,89] 
           it "solution2" $
               solution2 10 `shouldBe` [1,2,3,5,8,13,21,34,55,89] 

