module WeeklyTwo where

test1 = NodeOne 5 (NodeOne 3 Empty Empty Empty) (NodeOne 7 Empty Empty Empty) Empty
test2 = NodeTwo 5 10 (NodeTwo 1 4 Empty Empty Empty) (NodeTwo 7 8 Empty Empty Empty) (NodeTwo 11 14 Empty Empty Empty)
treestr = NodeOne "m" (NodeOne "a" Empty Empty Empty) (NodeOne "t" Empty Empty Empty) Empty
treestr2 = NodeOne "a" (NodeOne "b" (NodeOne "d" Empty Empty Empty) (NodeOne "e" Empty Empty Empty) Empty) (NodeOne "c" Empty Empty Empty) Empty
data TriTree a = Empty |
                 NodeOne a (TriTree a) (TriTree a) (TriTree a) |
                 NodeTwo a a (TriTree a) (TriTree a) (TriTree a)
                 deriving (Eq,Show)
-- Problem 1
    -- Attempts to find a given value in the given tree
    -- Takes a value and a TriTree as parameters
    -- Returns a boolean depending on if the value is present in the tree
search :: (Eq a, Ord a) => a -> TriTree a -> Bool
search _ tree@Empty = False
search v tree@(NodeOne val subLeft subMiddle subRight)
    | val == v = True
    | search v subLeft = True
    | otherwise = search v subMiddle
search v tree@(NodeTwo l r subLeft subMiddle subRight)
    | v == l = True
    | v == r = True
    | v < l = search v subLeft
    | v > r = search v subRight
    | otherwise = search v subMiddle

-- Problem 2
    -- Adds the value in the given TriTree
    -- Takes a value and a TriTree
    -- Returns a TriTree with the value added in the correct posistion
insert :: Ord a => a -> TriTree a -> TriTree a
insert v tree@Empty = NodeOne v Empty Empty Empty
insert v tree@(NodeOne val subLeft subMiddle subRight)
    | v > val = NodeOne val subLeft (insert v subMiddle) subRight 
    | otherwise = NodeOne val (insert v subLeft) subMiddle subRight

insert' :: Ord a => a -> a -> TriTree a -> TriTree a
insert' l r tree@Empty = NodeTwo l r Empty Empty Empty
insert' l r tree@(NodeTwo l2 r2 subLeft subMiddle subRight)
    | l > r2 = NodeTwo l2 r2 subLeft subMiddle (insert' l r subRight)
    | r < l2 = NodeTwo l2 r2 (insert' l r subLeft) subMiddle subRight
    | otherwise = NodeTwo l2 r2 subLeft (insert' l r subMiddle) subRight

-- Problem 3
    -- Adds each element from a list into the given TriTree
    -- Takes a list of values and a TriTree
    -- Returns the TriTree with all elements from the list inserted
insertList :: Ord a => [a] -> TriTree a -> TriTree a
insertList [] tree@(NodeOne val subLeft subMiddle subRight) = tree
insertList (x:xs) tree@Empty = insertList xs (insert x tree)
insertList (x:xs) tree@(NodeOne val subLeft subMiddle subRight) = insertList xs (insert x tree)

-- Problem 4
    -- Checks if both given TriTrees are identical
    -- Take 2 TriTress
    -- Returns a boolean depending on if the TriTree's are identical or not
--identical :: TriTree a1 -> TriTree a2 -> Bool
identical :: Eq a => TriTree a -> TriTree a -> Bool
identical tree@Empty tree1@Empty = True
identical tree@(NodeOne val subLeft subMiddle subRight) tree1@Empty = False
identical tree@Empty tree1@(NodeOne val subLeft subMiddle subRight) = False
identical tree@(NodeOne val subLeft subMiddle subRight) tree1@(NodeOne val2 subLeft2 subMiddle2 subRight2)
    | val /= val2 = False
    | identical subLeft subLeft2 && identical subMiddle subMiddle2 = True
    | otherwise = False


identical tree@(NodeTwo val v subLeft subMiddle subRight) tree1@Empty = False
identical tree@Empty tree1@(NodeTwo val v subLeft subMiddle subRight) = False
identical tree@(NodeTwo val v subLeft subMiddle subRight) tree1@(NodeTwo val2 v2 subLeft2 subMiddle2 subRight2)
    | val /= val2 || v /= v2 = False
    | identical subLeft subLeft2 && identical subRight subRight2 && identical subMiddle subMiddle2 = True
    | otherwise = False

-- Problem 5
    -- Applies a given function to every value in the TriTree
    -- Takes a function and TriTree
    -- Returns a TriTree with the given function applied to each node
treeMap :: (a -> a) -> TriTree a -> TriTree a
treeMap _ tree@Empty = Empty
treeMap f tree@(NodeOne val subLeft subMiddle subRight) = NodeOne (f val) (treeMap f subLeft) 
                                                                            (treeMap f subMiddle)
                                                                            subRight 
treeMap f tree@(NodeTwo l r subLeft subMiddle subRight) = NodeTwo (f l) (f r) (treeMap f subLeft) 
                                                                                (treeMap f subMiddle)
                                                                                (treeMap f subRight)

-- Problem 6
    -- Combines the TriTree in PreOrder while using the inital value and the given function
    -- Takes a function, inital value and a TriTree
    -- Returns a value of the combined nodes through the given function
treeFoldPreOrder :: (t1 -> t2 -> t1) -> t1 -> TriTree t2 -> t1
treeFoldPreOrder _ n Empty = n
treeFoldPreOrder f n tree@(NodeOne val subLeft subMiddle subRight) = 
    --treeFoldPreOrder f (treeFoldPreOrder f (f n val) subLeft) subMiddle
    treeFoldPreOrder f (treeFoldPreOrder f (f n val) subLeft) subMiddle

treeFoldPreOrder f n (NodeTwo l r left mid right) =
        treeFoldPreOrder f (treeFoldPreOrder f (treeFoldPreOrder f (f (f n l) r) left) mid) right 

-- Problem 7
    -- Combines the TriTree in InOrder while using the inital value and the given function
    -- Takes a function, inital value and a TriTree
    -- Returns a value of the combined nodes through the given function
treeFoldInOrder :: (t1 -> t2 -> t1) -> t1 -> TriTree t2 -> t1
treeFoldInOrder _ n Empty = n
treeFoldInOrder f n tree@(NodeOne val subLeft subMiddle subRight) = 
    --treeFoldInOrder f (f n (treeFoldInOrder f val subLeft)) subMiddle
    treeFoldInOrder f (f (treeFoldInOrder f n subLeft) val) subMiddle

treeFoldInOrder f n (NodeTwo l r left mid right) = 
        treeFoldInOrder f (treeFoldInOrder f (f (f (treeFoldInOrder f n left) l) r) mid) right

-- Problem 8
    -- Combines the TriTree in PostOrder while using the inital value and the given function
    -- Takes a function, inital value and a TriTree
    -- Returns a value of the combined nodes through the given function
treeFoldPostOrder :: (t -> a -> t) -> t -> TriTree a -> t
treeFoldPostOrder _ n Empty = n
treeFoldPostOrder f n tree@(NodeOne val subLeft subMiddle subRight) = 
    -- treeFoldPostOrder f (f n (treeFoldPostOrder f val subMiddle)) subLeft
    f (treeFoldPostOrder f (treeFoldPostOrder f n subLeft) subMiddle) val

treeFoldPostOrder f n (NodeTwo l r left mid right) = 
        f (treeFoldPostOrder f n (NodeOne l left mid right)) r 