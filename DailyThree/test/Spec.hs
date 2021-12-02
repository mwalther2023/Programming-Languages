import Test.Hspec
import DailyThree

main :: IO ()
main = hspec $ do
    describe "DailyOne" $ do
        it "\nTesting removeAllExcept:\n\t" $
            (removeAllExcept 'a' ['b','a','c','a']) `shouldBe` "aa"
        it "\n\t" $
            (removeAllExcept 1 [2,3,4,1]) `shouldBe` [1]
        it "\n\t" $
            (removeAllExcept 0 [2,3,4,1]) `shouldBe` []
        
        it "\nTesting countOccurences:\n\t" $
            (countOccurrences 1  [2,4,5,2]) `shouldBe` 0
        it "\n\t" $
            (countOccurrences 'a' ['a', 'b', 'a', 'c']) `shouldBe` 2
        it "\n\t" $
            (countOccurrences 'b' ['a', 'b', 'a', 'c']) `shouldBe` 1

        it "\nTesting substitute :\n\t" $
            (substitute  3 4 [1, 2, 3, 4]) `shouldBe` [1, 2, 4, 4]
        it "\n\t" $
            (substitute  1 2 [1,1,1,1]) `shouldBe` [2,2,2,2]
        it "\n\t" $
            (substitute  1 2 [3,1,1,1]) `shouldBe` [3,2,2,2]