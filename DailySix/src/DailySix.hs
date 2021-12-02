module DailySix where

-- Problem 1
    -- To remove all strings in a list that are greater in length than the given integer
    -- Takes an integer and a list of strings
    -- returns a list of strings whose length is less than or equal to the given integer
shorterThan :: Int -> [String] -> [String]
shorterThan y xs = filter (\n -> length n <= y) xs

-- Problem 2
    -- Removes integers from the list that are multiples of the given integer
    -- Takes an integer and a list of integers
    -- Returns a list where all multiples of the given integer are removed
removeMultiples :: Integral a => a -> [a] -> [a]
removeMultiples num xs = filter (\n -> n `mod` num /= 0) xs

-- Problem 3
    -- removes all Nothing elements in the list
    -- Takes a list of Just and Nothings
    -- Returns a list where all elements are not Nothing
onlyJust :: Eq a => [Maybe a] -> [Maybe a]
onlyJust xs = filter (\n -> n /= Nothing) xs