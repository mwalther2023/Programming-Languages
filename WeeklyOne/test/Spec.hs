import Test.Hspec
import WeeklyOne

main :: IO ()
main = hspec $ do
    describe "DailyOne" $ do
        it "\nTesting removeChar:\n\t" $
            (removeChar 'a' "abcd") `shouldBe` "bcd"
        
        
        it "\nTesting removeWhitespace:\n\t" $
            (removeWhitespace "Hello World") `shouldBe` "HelloWorld"
        

        it "\nTesting removePunctuation :\n\t" $
            (removePunctuation  "It's over there. (Is it though)") `shouldBe` "It's over there Is it though"
        
        it "\nTesting charsToAscii :\n\t" $
            (charsToAscii  "aaa") `shouldBe` [97,97,97]
        
        it "\nTesting asciiToChars :\n\t" $
            (asciiToChars  [97,97,97]) `shouldBe` "aaa"

        it "\nTesting shiftInts :\n\t" $
            (shiftInts  1 [2,4,6]) `shouldBe` [3,5,7]
        it "\nTesting shiftInts :\n\t" $
            (shiftInts  1 [0,1,17,50,63,126,127]) `shouldBe` [1,2,18,51,64,127,0]

        it "\nTesting shiftMessage :\n\t" $
            (shiftMessage  1 "aaa") `shouldBe` "bbb"
        