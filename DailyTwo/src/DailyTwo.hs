{-# LANGUAGE FlexibleContexts #-}
module DailyTwo where
    -- Matthew Walther
    -- Daily Two Homework

--Problem 1
    -- Finding and returning every 3rd element in a given list
    -- This function takes a single list parameter
    -- This function returns a list of all elements from every 3rd posistion in the list
everyThird xs = thirdHelper 1 xs
thirdHelper _ [] = []
thirdHelper 3 (x : xs) = x : thirdHelper 1 xs
thirdHelper n (_ : xs) = thirdHelper (n + 1) xs

--Problem 2
    -- Calculating the dot product of 2 given lists
    -- This function takes 2 list parameters
    -- The function's result is the dot product of the given lists
tupleDotProduct [] [] = 0
tupleDotProduct xs ys = dotHelper xs ys 0

dotHelper xs ys n = (xs !! n)*(ys !! n)  + if n+1 < length xs then dotHelper xs ys (n+1) else 0
--Problem 3
    -- Concatinate a string to all strings in a given list
    -- This function takes a string and a list of strings
    -- The function's result is the combined string of each element in the list with the given string
appendToEach a xs = map (++ a) xs

