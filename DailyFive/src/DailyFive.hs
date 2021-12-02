module DailyFive where
import Data.Char


-- Problem 1
    -- Produces a list of products from each pair
    -- It takes a list of integer pairs
    -- It returns a list of the product of each pair
--multPairs :: Num a => [(a, a)] -> [a]
multPairs :: [(Integer, Integer)] -> [Integer]
multPairs [] = []
multPairs [(x,y)] = map (x*) [y]
multPairs ((x,y):xs) = multPairs [(x,y)] ++ multPairs xs
--Problem 2
    -- Produces the pair of an integer and its square for every element in the list
    -- It takes a list of integers as the parameter
    -- It returns a list of pairs which are the original integer of the list and its square
squareList :: Num b => [b] -> [(b, b)]
squareList xs = map (square) xs
square x = (x,x*x)

--Problem 3
    -- Produces a list of Bool that represent if the position in the list was a lowercase char or not of the original string
    -- It takes a list of String as a parameter
    -- It returns a list of Bool where each True corresponds to a String starting with a lowercase letter and false otherwise
findLowercase :: [String] -> [Bool]
findLowercase [] = []
findLowercase (x:xs) = if head(map (isLower) x)
    then True:findLowercase xs
    else False:findLowercase xs