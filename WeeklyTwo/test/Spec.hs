import Test.Hspec
import WeeklyTwo

main :: IO ()
main = hspec $ do
    describe "DailyThere" $ do
        it "\nTesting search :\n\t" $
            (search  5 test1) `shouldBe` True
        it "\nTesting search :\n\t" $
            (search  1 test2) `shouldBe` True

        it "\nTesting insert  :\n\t" $
            (insert 4 test1) `shouldBe` NodeOne 5 (NodeOne 3 Empty (NodeOne 4 Empty Empty Empty) Empty) (NodeOne 7 Empty Empty Empty) Empty

        it "\nTesting insertList :\n\t" $
            (insertList [1,2,4] test1) `shouldBe` NodeOne 5 (NodeOne 3 (NodeOne 1 Empty (NodeOne 2 Empty Empty Empty) Empty) (NodeOne 4 Empty Empty Empty) Empty) (NodeOne 7 Empty Empty Empty) Empty
        
        it "\nTesting identical :\n\t" $
            (identical test1 test1) `shouldBe` True
        it "\nTesting identical :\n\t" $
            (identical test1 (insert 4 test1)) `shouldBe` False

        it "\nTesting treeMap :\n\t" $
            (treeMap (+1) test1) `shouldBe` NodeOne 6 (NodeOne 4 Empty Empty Empty) (NodeOne 8 Empty Empty Empty) Empty
        
        it "\nTesting treeFoldPreOrder :\n\t" $
            (treeFoldPreOrder (+) 1 test1) `shouldBe` 16

        it "\nTesting treeFoldInOrder :\n\t" $
            (treeFoldInOrder (+) 1 test1) `shouldBe` 16
        
        it "\nTesting treeFoldPostOrder :\n\t" $
            (treeFoldPostOrder (+) 1 test1) `shouldBe` 16