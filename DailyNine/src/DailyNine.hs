module DailyNine where

-- Problem 1
    -- Finds the max and min of the list in 3(n/2)
    -- Takes a list of values
    -- Returns the list's min and max values as a tuple pair

minAndMax :: (Num b, Ord b) => [b] -> (b, b)
minAndMax [] = (0,0)
minAndMax (x:xs)
    | x < xs !! 1 = q1Helper (x,xs !! 1) xs
    | otherwise = q1Helper (xs !! 1, x) xs
q1Helper :: Ord a => (a, a) -> [a] -> (a, a)
q1Helper (a,b) [] = (a,b)
q1Helper (a,b) (x:xs)
  -- | a > b = q1Helper (b,a) (x:xs)
  | x < a = q1Helper (x,b) xs
  | x > b = q1Helper (a,x) xs
  | otherwise = q1Helper (a,b) xs


-- Problem 2
    -- Create a list containing the elements at every k place
    -- Takes an integer and a list
    -- Returns a list containing all elements at every k positions
everyK :: (Eq t, Num t) => t -> [a] -> [a]
everyK k xs = kHelper 1 xs k
kHelper :: (Eq t, Num t) => t -> [a] -> t -> [a]
kHelper _ [] k = []
kHelper n (x : xs) k = if k == n then x : kHelper 1 xs k else kHelper (n + 1) xs k


-- Problem 3
    -- Combines 2 lists taking them one at a time from each list
    -- Takes 2 lists of values
    -- Returns one list of the combined values where one was taken at a time from each list
shuffle :: [a] -> [a] -> [a]
shuffle [] [] = []
shuffle xs [] = xs
shuffle [] ys = ys
shuffle (x:xs) (y:ys) = x:y:shuffle xs ys