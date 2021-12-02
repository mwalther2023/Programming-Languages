{-# LANGUAGE DataKinds #-}
module DailyFour where

-- Problem 1
    -- Combines 3 lists into a list of 3 tuples where each tuple contains the elemtents in the same position from the lists
    -- This function takes 3 list parameters
    -- This function returns a list of tuples where each tuples is the set of elements from the same position in the parameter lists
zip3Lists [] [] [] = []
zip3Lists [_] [] [] = []
zip3Lists [] [_] [] = []
zip3Lists [] [] [_] = []
zip3Lists (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3Lists xs ys zs

-- Problem 2
    -- Takes a list of tuples to revert them to their original form before being zipped
    -- It takes a list of 3 triple tuples as the parameter
    -- It returns a tuple of 3 triple lists where each position from the tuples has been put together
unzipTriples :: [(a1, a2, a3)] -> ([a1], [a2], [a3])
unzipTriples [] = ([],[],[])
-- unzipTriples [(a1,a2,a3),(b1,b2,b3),(c1,c2,c3)] = ([a1,b1,c1],[a2,b2,c2],[a3,b3,c3])
unzipTriples ((a,b,c):xs) = (a:as,b:bs,c:cs)
    where (as, bs, cs) = unzipTriples xs
-- Problem 3
    -- Merges sorted lists into one sorted lists
    -- It takes 3 sorted lists as the parameters
    -- It returns one sorted list
mergeSorted3 :: Ord a => [a] -> [a] -> [a] -> [a]
mergeSorted3 [] [] [] = []
mergeSorted3 [x] [] [] = [x]
mergeSorted3 [] [x] [] = [x]
mergeSorted3 [] [] [x] = [x]
mergeSorted3 xs ys zs = merge (merge xs ys) zs

merge :: Ord a => [a] -> [a] -> [a]
merge [] x = x
merge x [] = x
merge (x:xs) (y:ys) | y < x     = y : merge (x:xs) ys
merge (x:xs) (y:ys) | otherwise = x : merge xs (y:ys)