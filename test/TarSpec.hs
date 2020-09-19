module TarSpec where

import Test.Hspec 
import Tar

spec :: Spec
spec =
    describe "Tartaglia" $ do
        it "solution" $
            take 6 tar `shouldBe` [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
        it "solution2" $
            take 6 tar2 `shouldBe` [[1],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
        it "solution3" $
            take 6 tart `shouldBe` [[],[1,1],[1,2,1],[1,3,3,1],[1,4,6,4,1],[1,5,10,10,5,1]]
        it "return next tartaglia line" $ do
            nextT [1] `shouldBe` [1,1] 
            nextT [1,1] `shouldBe` [1,2,1] 
            nextT [1,2,1] `shouldBe` [1,3,3,1] 
            nextT [1,3,3,1] `shouldBe` [1,4,6,4,1] 
        it "return next tartaglia line (version2)" $ do
            nextT2 [1] `shouldBe` [1,1] 
            nextT2 [1,1] `shouldBe` [1,2,1] 
            nextT2 [1,2,1] `shouldBe` [1,3,3,1] 
            nextT2 [1,3,3,1] `shouldBe` [1,4,6,4,1] 



