import Test.Hspec
import DailySix
main :: IO ()
main = hspec $ do
    describe "DailyThere" $ do
        it "\nTesting shorterThan :\n\t" $
            (shorterThan  5 ["hi","Hello","Hello There"]) `shouldBe` ["hi","Hello"]
        it "\n\t" $
            (shorterThan  3 ["hi","Hello","Hello There"]) `shouldBe` ["hi"]
        it "\n\t" $
            (shorterThan  11 ["hi There","Hello","Hello There"]) `shouldBe` ["hi There","Hello","Hello There"]
                                            
        it "\nTesting removeMultiples :\n\t" $
            (removeMultiples 5 [1,3,5,10]) `shouldBe` [1,3]
        it "\n\t" $
            (removeMultiples 2 [1,2,4,10]) `shouldBe` [1]
        it "\n\t" $
            (removeMultiples 3 [1,3,5,9]) `shouldBe` [1,5]

        it "\nTesting onlyJust :\n\t" $
            (onlyJust [Nothing, Just 5, Nothing, Just 10]) `shouldBe` [Just 5, Just 10]
        it "\n\t" $
            (onlyJust [Just 0, Just 5, Nothing, Just 10]) `shouldBe` [Just 0, Just 5, Just 10]
        it "\n\t" $
            (onlyJust [Nothing, Nothing, Nothing, Just 10]) `shouldBe` [Just 10]