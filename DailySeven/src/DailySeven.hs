module DailySeven where

--Problem 1
    --Combines a list of lists into one list
    --Takes a list of lists as a parameter
    --Returns a single list containing all elements
createOneList :: [[a]] -> [a]
createOneList [] = []
createOneList xs = foldl (++) [] xs


--Problem 2
    --Finds the largest number in a list of numbers
    --Takes a list of numbers
    --Returns the maximum value out of the elements
findLargest [] = 0
findLargest xs = foldr (\x y -> if x > y then x else y) 0 xs

--Problem 3
    --Says whether the list is all True's or not
    --Takes a list of booleans
    --Returns a boolean based on if the list is all True's or not
allTrue [] = True
allTrue xs = foldr (\x y -> if x == y then x else False) True xs