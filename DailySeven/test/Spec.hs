import Test.Hspec
import DailySeven
main :: IO ()
main = hspec $ do
    describe "DailyThere" $ do
        it "\nTesting createOneList :\n\t" $
            (createOneList [ [1,2], [3], [ ], [4, 5] ]) `shouldBe` [1,2,3,4,5]
        it "\n\t" $
            (createOneList [ [1], [3], [2], [4, 5] ]) `shouldBe` [1,3,2,4,5]
        it "\n\t" $
            (createOneList [ [1], [3], [ ], [4, 5] ]) `shouldBe` [1,3,4,5]
                                            
        it "\nTesting findLargest :\n\t" $
            (findLargest [1,3,5,10]) `shouldBe` 10
        it "\n\t" $
            (findLargest [1,3,15,10]) `shouldBe` 15
        it "\n\t" $
            (findLargest [11,3,5,10]) `shouldBe` 11

        it "\nTesting allTrue :\n\t" $
            (allTrue [True, True, False, True]) `shouldBe` False
        it "\n\t" $
            (allTrue [True, True, True]) `shouldBe` True
        it "\n\t" $
            (allTrue [False, False, True]) `shouldBe` False