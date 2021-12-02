import Test.Hspec
import DailyFour
main :: IO ()
main = hspec $ do
    describe "DailyThere" $ do
        it "\nTesting zip3Lists :\n\t" $
            (zip3Lists  [1, 2, 3] ['a', 'b', 'c'] [4, 5, 6]) `shouldBe` 
                                            [(1, 'a', 4), (2, 'b', 5), (3, 'c', 6)]
        it "\n\t" $
            (zip3Lists  ['a', 'b', 'c'] [4, 5, 6] [1, 2, 3] ) `shouldBe` 
                                            [('a', 4, 1), ('b', 5, 2), ('c', 6, 3)]
        it "\n\t" $
            (zip3Lists  ['a', 'b', 'c'] [4, 5, 6] ['d', 'e', 'f'] ) `shouldBe` 
                                            [('a', 4, 'd'), ('b', 5, 'e'), ('c', 6, 'f')]

        it "\nTesting unzipTriples:\n\t" $
            (unzipTriples [ (1,2,3), (4, 5, 6), (7, 8, 9) ]) `shouldBe` 
                                            ( [1,4,7], [2, 5, 8], [3, 6, 9] )
        it "\n\t" $
            (unzipTriples [ (10, 11, 12), (4, 5, 6), (7, 8, 9) ]) `shouldBe` 
                                            ( [10,4,7], [11, 5, 8], [12, 6, 9] )
        it "\n\t" $
            (unzipTriples [ (1,2,3), (4, 5, 6), (10, 11, 12) ]) `shouldBe` 
                                            ( [1,4,10], [2, 5, 11], [3, 6, 12] )

        it "\nTesting mergeSorted3 :\n\t" $
            (mergeSorted3  [2, 3, 5] [1, 8] [-1, 0, 4, 10] `shouldBe` 
                                            [-1, 0, 1, 2, 3, 4, 5, 8, 10])
        it "\n\t" $
            (mergeSorted3  [-4,-3,-2,-1] [0,1,2,3] [4] `shouldBe` 
                                            [-4,-3,-2,-1,0,1,2,3,4])
        it "\n\t" $
            (mergeSorted3  [1,3,5,7] [2,4,6] [0,8,9,10] `shouldBe` 
                                            [0,1,2,3,4,5,6,7,8,9,10])