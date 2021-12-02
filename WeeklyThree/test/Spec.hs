import Test.Hspec
import WeeklyThree


isOdd :: Integer -> Maybe Integer
isOdd x = if (x `mod` 2 == 1)
            then Just x
            else Nothing


isA :: Char -> Maybe Char
isA x = case x of
    'a' -> Just x
    'A' -> Just x
    (_) -> Nothing


oddTimes123 :: Integer -> Maybe [Integer]
oddTimes123 x = if (x `mod` 2 == 1)
                then Just [x*1, x*2, x*3]
                else Nothing

main = hspec $ do
    describe "firstAnswer" $ do
        context "\n\t" $ do
            it "\n\t" $ do
                firstAnswer (isOdd) [] `shouldBe` Nothing
            it "\n\t" $ do
                firstAnswer (isOdd) [2] `shouldBe` Nothing
            
            it "" $ do
                firstAnswer (isOdd) [3] `shouldBe` Just 3
            
        context "" $ do
            it "" $ do
                firstAnswer (isA) ['b'] `shouldBe` Nothing
            it "" $ do
                firstAnswer (isA) ['b'..'z'] `shouldBe` Nothing
            it "" $ do
                firstAnswer (isA) ['A'] `shouldBe` Just 'A'
            

    describe "allAnswers" $ do
        context "\n\t" $ do
            it "\n\t" $ do
                allAnswers (oddTimes123) [] `shouldBe` Just []
            it "\n\t" $ do
                allAnswers (oddTimes123) [1] `shouldBe` Just [1,2,3]
            it "\n\t" $ do
                allAnswers (oddTimes123) [2] `shouldBe` Nothing
            it "\n\t" $ do
                allAnswers (oddTimes123) [1,3] `shouldBe` Just [1,2,3,3,6,9]
            it "\n\t" $ do
                allAnswers (oddTimes123) [1,2,3] `shouldBe` Nothing
            
    
    describe "checkPat" $ do
        context "\n\t" $ do
            it "\n\t" $ do
                checkPat (VariablePat "Hi") `shouldBe` True
            it "\n\t" $ do
                checkPat (VariablePat "") `shouldBe` True
        context "" $ do
            it "" $ do
                checkPat (ConstructorPat ("test", VariablePat "test")) `shouldBe` False
            it "" $ do
                checkPat (ConstructorPat ("test", VariablePat "Other test")) `shouldBe` True
        context "" $ do
            it "" $ do
                checkPat (TuplePat []) `shouldBe` False
            it "" $ do
                checkPat (TuplePat [VariablePat "Hi", VariablePat "Hello"]) `shouldBe` True

    describe "match" $ do
        context "\n\t" $ do
            it "\n\t" $ do
                match (Constant (1), WildcardPat) `shouldBe` Just [("", Constant 1)]
            it "\n\t" $ do 
                match (Tuple [Constant (1)], WildcardPat) `shouldBe` Just [("",Tuple [Constant 1])]
        context "" $ do
            it "" $ do
                match (Constant (1), VariablePat "test") `shouldBe` Just [("test",Constant 1)]
            it "" $ do
                match (Unit, VariablePat "test") `shouldBe` Just [("test", Unit)]
        context "" $ do
            it "" $ do
                match (Unit, UnitPat) `shouldBe` Just [("", Unit)]
            it "" $ do
                match (Constant (1), UnitPat) `shouldBe` Nothing
        context "" $ do
            it "" $ do
                match (Constant (1), ConstantPat (1)) `shouldBe` Just [("", Constant 1)]
            it "" $ do
                match (Constant (1), ConstantPat 2) `shouldBe` Nothing
        context "" $ do
            it "" $ do
                match (Constructor ("test", Constant (1)), ConstructorPat ("test", ConstantPat (1))) `shouldBe` Just [("test",Constructor ("test",Constant 1)),("",Constant 1)]
            it "" $ do
                match (Constructor ("test", Constant (1)), ConstructorPat ("test", ConstantPat (2))) `shouldBe` Nothing
        context "TuplePat" $ do 
            it "" $ do 
                match (Tuple [Constant (1)], TuplePat [ConstantPat(1)]) `shouldBe` Just [("",Constant 1)]
            it "" $ do 
                match (Tuple [Constant (1)], TuplePat [ConstantPat(2)]) `shouldBe` Nothing
            
    
    describe "firstMatch" $ do
        it "\n\t" $ do
            firstMatch Unit [ConstantPat 1, UnitPat] `shouldBe` Just [("",Unit)]
        it "\n\t" $ do
            firstMatch Unit [ConstantPat 1] `shouldBe` Nothing