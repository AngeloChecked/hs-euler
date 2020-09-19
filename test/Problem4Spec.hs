module Problem4Spec where

import Test.Hspec
import Problem4
import Data.Ord
import qualified Data.Map as MAP

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
               solution 2 `shouldBe` (9009, 99, 91) 
            it "with 3 digit" $ do
               solution 3 `shouldBe` (906609,993,913)
            it "solution2: with 3 digit" $ do
               solution2 `shouldBe`  (906609,913,993)
        describe "paolino" $ do
            it "digit of a number" $
                floor (logBase 10 40005::Double)+1 `shouldBe` (5::Int)
            it "reverse a number" $ do 
                reverseN 3213231 `shouldBe` 1323123
                reverseN2 3213231 `shouldBe` 1323123
            it "for each number apply the functione in the list and move it to the head of tuple" $ do
                selectf (\x -> if x `mod` 2 /= 0 then (Just (x,x^2)) else Nothing) [1..5] `shouldBe` [(1,[1,2,3,4,5]),(3,[1,2,9,4,5]),(5,[1,2,3,4,25])] 
                selectf (\el -> if length el > 0 then (Just (head el, tail el)) else Nothing)  [([]::[Int]),[],[]] `shouldBe` [] 
            it "head of the tuple is the bigest number combination" $ do 
                step (comparing $ uncurry (*)) [[(99,97)],[(98,98),(98,97),(98,96)],[(97,97),(97,96),(97,95)]] `shouldBe` Just ((98,98),[[(99,97)],[(98,97),(98,96)],[(97,97),(97,96),(97,95)]]) 
                step (comparing $ uncurry (*)) [[]] `shouldBe` Nothing 
            it "return number combinations ordered by biggest number of all list heads" $
                driveStep (comparing $ uncurry (*)) [[(99,97)],[(98,98),(98,97),(98,96)],[(97,97),(97,96),(97,95)]] `shouldBe` [(98,98),(99,97),(98,97),(97,97),(98,96),(97,96),(97,95)]
            it "produce all progressive combinations, for each list the header is the biggest" $
                (fmap (take 3) $ take 2 (prods 3)) `shouldBe` [[(999,999),(999,998),(999,997)],[(998,998),(998,997),(998,996)]]
            it "get each progressive combination from bigger to smaller" $
                (take 5 $ (\(x,y) -> (x, y , x * y)) <$> (enumerate 3))  `shouldBe` [(999,999,998001),(999,998,997002),(998,998,996004),(999,997,996003),(998,997,995006)] 
            it "psolution" $
                paolinoSolution `shouldBe` [(993,913,906609),(962,924,888888),(968,916,886688),(932,924,861168),(957,894,855558),(982,869,853358)]
            


             

