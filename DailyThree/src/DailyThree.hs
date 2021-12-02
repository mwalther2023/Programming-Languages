module DailyThree where
    --Matthew Walther
    --DailyThree Homework

--Problem 1
    --Removes everything except the given value in a list
    --It takes a value and a list
    --This function returns the list will all values except the given one removed

removeAllExcept c [] = []
removeAllExcept c (x:xs) = if c /= x
                            then removeAllExcept c xs
                        else x : removeAllExcept c xs

--Problem 2
    --Counts the times a given value has occured in a list
    --It takes a value and a list
    --It returns an Integer representing the number of times the given value is present in the list
countOccurrences  :: Eq a => a -> [a] -> Int
countOccurrences  c [] = 0
countOccurrences  c (x:xs) = if c == x
                            then 1 + countOccurrences  c xs
                        else countOccurrences  c xs


--Problem 3
    --Replaces the first variable in a given list with the second variable
    --It takes 2 seperate variables and a list
    --This function returns a list with all occurences of the first variable replaced by the second variable

substitute c1 c2 [] = []
substitute c1 c2 (x:xs) = if c1 == x
                            then c2 : substitute c1 c2 xs
                        else x : substitute c1 c2 xs