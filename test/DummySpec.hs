module DummySpec where

import Test.Hspec

spec :: Spec
spec = 
    describe "" $
        it "" $
            1 `shouldBe` 1

