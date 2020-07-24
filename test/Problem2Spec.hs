module Problem2Spec where
    
import Test.Hspec
import Problem2

spec :: Spec
spec = 
    describe "problem 2" $
        it "fibonacci sequence starting with 1 end 2" $
           take 10 problem `shouldBe` [1,2,3,5,8,13,21,34,55,89] 
