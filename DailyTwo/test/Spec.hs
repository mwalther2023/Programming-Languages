import Test.Hspec
import DailyTwo
main :: IO ()
main = hspec $ do
    describe "DailyTwo" $ do
        it "\nTesting everyThird :\n\t" $
            (everyThird [1, 2, 3, 4, 5, 6]) `shouldBe` [3, 6]
        it "\n\t" $
            (everyThird [0, 1, 2, 3, 4, 5, 6]) `shouldBe` [2, 5]
        it "\n\t" $
            (everyThird [0, 1, 2]) `shouldBe` [2]

        it "\nTesting tupleDotProduct :\n\t" $
            (tupleDotProduct [] []) `shouldBe` 0
        it "\n\t" $
            (tupleDotProduct [0, 1] [2,3]) `shouldBe` 3
        it "\n\t" $
            (tupleDotProduct [0, 2] [3, 5]) `shouldBe` 10

        it "\nTesting appendToEach :\n\t" $
            (appendToEach "!!!" [ "Hello", "Goodbye" ]) `shouldBe` [ "Hello!!!", "Goodbye!!!" ]
        it "\n\t" $
            (appendToEach "a" [ "a", "b" ]) `shouldBe` [ "aa", "ba" ]
        it "\n\t" $
            (appendToEach "Hi!" [ "a", "b" ]) `shouldBe` [ "aHi!", "bHi!" ]