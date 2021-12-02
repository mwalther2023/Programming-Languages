import Test.Hspec
import DailyEight
main :: IO ()
main = hspec $ do
    describe "DailyEight" $ do
        it "\n\t" $
            (inYear 2000 list) `shouldBe` [Event {name = "New Year", day = 1, month = "Jan", year = 2000, xlocation = 150.0, ylocation = 150.0},Event {name = "Flowers", day = 10, month = "May", year = 2000, xlocation = 100.0, ylocation = 100.0}]
        it "\n\t" $
            (inYear 2001 list) `shouldBe` [Event {name = "Party", day = 17, month = "Mar", year = 2001, xlocation = 120.0, ylocation = 120.0}]
        it "\n\t" $
            (inYear 2005 list) `shouldBe` []

        it "\nTesting inDayRange :\n\t" $
            (inDayRange 5 17 list) `shouldBe` ["Party","Flowers"]
        it "\n\t" $
            (inDayRange 1 5 list) `shouldBe` ["New Year"]
        it "\n\t" $
            (inDayRange 3 5 list) `shouldBe` []

        it "\nTesting inArea :\n\t" $
            (inArea "Flowers" 90 95 90 95 list) `shouldBe` []
        it "\n\t" $
            (inArea "New Year" 90 95 90 95 list) `shouldBe` []
        it "\n\t" $
            (inArea "Party" 90 125 90 125 list) `shouldBe` [Event {name = "Party", day = 17, month = "Mar", year = 2001, xlocation = 120.0, ylocation = 120.0}]