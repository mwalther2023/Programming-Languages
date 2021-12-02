import Test.Hspec
import DailyNine
main :: IO ()
main = hspec $ do
    describe "DailyNine" $ do
        it "\nTesting minAndMax :\n\t" $
            (minAndMax [1,2,3,4]) `shouldBe` (1,4)
        it "\n\t" $
            (minAndMax [1,-2,3,14]) `shouldBe` (-2,14)
        it "\n\t" $
            (minAndMax [15,2,3,46]) `shouldBe` (2,46)
        
        it "\nTesting everyK :\n\t" $
            (everyK 1 [1,2,3,4,5,6]) `shouldBe` [1,2,3,4,5,6]
        it "\n\t" $
            (everyK 3 [1,2,3,4,5,6]) `shouldBe` [3,6]
        it "\n\t" $
            (everyK 5 [1,2,3,4,5,6]) `shouldBe` [5]

        it "\nTesting shuffle :\n\t" $
            (shuffle [1,3,5] [2,4]) `shouldBe` [1,2,3,4,5]
        it "\n\t" $
            (shuffle [3,6,9] [2,4,6]) `shouldBe` [3,2,6,4,9,6]
        it "\n\t" $
            (shuffle [1,2,3] [4,5,6]) `shouldBe` [1,4,2,5,3,6]