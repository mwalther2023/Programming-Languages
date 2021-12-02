import Test.Hspec
import DailyFive
main :: IO ()
main = hspec $ do
    describe "DailyThere" $ do
        it "\nTesting zip3Lists :\n\t" $
            (multPairs  [(1,1),(2,3)]) `shouldBe` [1,6]
        it "\n\t" $
            (multPairs  [(1,2),(3,3)]) `shouldBe` [2,9]
        it "\n\t" $
            (multPairs  [(2,2),(4,4)]) `shouldBe` [4,16]

        it "\nTesting squareList :\n\t" $
            (squareList [1,2,3]) `shouldBe` [(1,1),(2,4),(3,9)]
        it "\n\t" $
            (squareList [4,5,6]) `shouldBe` [(4,16),(5,25),(6,36)]
        it "\n\t" $
            (squareList [0,7,9]) `shouldBe` [(0,0),(7,49),(9,81)]

        it "\nTesting findLowercase :\n\t" $
            (findLowercase ["hi","Hello"]) `shouldBe` [True,False]
        it "\n\t" $
            (findLowercase ["Hi","Hello"]) `shouldBe` [False,False]
        it "\n\t" $
            (findLowercase ["world","hello"]) `shouldBe` [True,True]
