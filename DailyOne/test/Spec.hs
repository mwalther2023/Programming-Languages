import Test.Hspec
import DailyOne

main :: IO ()
main = hspec $ do
    describe "DailyOne" $ do
        it "\nTesting quadratic:\n\tIf a = 1, b = 2, c = 3, and x = 4, then \n\t1 + 2(4) + 3(4^2) = 57" $
            (quadratic 1 2 3 4) `shouldBe` 57
        it "\n\tIf a = 0, b = 0, c = 0, and x = 0, then \n\t0 + 0(0) + 0(0^2) = 0" $
            (quadratic 0 0 0 0) `shouldBe` 0
        
        it "\nTesting scaleVector:\n\tScaling the vector (3, 4) by 5 results in the new vector (15, 20)" $
            (scaleVector 5 (3,4)) `shouldBe` (15, 20)
        it "\n\tScaling the vector (0, 0) by 0 results in the new vector (0, 0)" $
            (scaleVector 0 (0,0)) `shouldBe` (0, 0)
        it "\nTesting tripleDistance:\n\tThe 3D points (2, 1, 1) and (1, 1, 1) should result in 1.0" $
            (tripleDistance (2,1,1) (1,1,1)) `shouldBe` 1.0
        it "\n\tThe 3D points (0, 0, 1) and (0, 0, 0) should result in 1.0" $
            (tripleDistance (0,0,1) (0,0,0)) `shouldBe` 1.0