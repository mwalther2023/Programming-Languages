module WeeklyThree where

data Pattern = WildcardPat | VariablePat (String) | UnitPat | ConstantPat (Int) | ConstructorPat (String, Pattern) | TuplePat ([Pattern])
    deriving (Eq, Show)
data Value = Constant (Int) | Unit | Constructor (String, Value) | Tuple [Value] deriving (Eq, Show)

patt = TuplePat [VariablePat "Hi",VariablePat "Hi"]

-- Problem 1
    -- Apply function to each element in list to find first Just v
    -- Takes a function and list
    -- Returns first Just v from applying function or Nothing is no Just
firstAnswer :: ( a -> Maybe b ) -> [a] -> Maybe b
firstAnswer _ [] = Nothing
firstAnswer f (x:xs) = case f x of
    Just v -> Just v
    Nothing -> firstAnswer f xs

-- Problem 2
    -- Apply a function to list and makes sure all are Just
    -- Takes a function and List
    -- Returns Just List or Nothing is there is a Nothing present
allAnswers :: (a -> Maybe [b]) -> [a] -> Maybe [b]
allAnswers _ [] = Just []
allAnswers f (x:xs) = case (f x) of
    Nothing -> Nothing
    Just v -> fmap (++) (Just v) <*> allAnswers f xs 
    
-- Problem 3
    -- Checks the pattern to make sure they all match
    -- Takes pattern
    -- Returns Bool, True is all patterns match and false otherwise
checkPat :: Pattern -> Bool
checkPat x = checkDuplicate (checkHelper x)

checkHelper :: Pattern -> [String]
checkHelper x = val where
    val = case x of
        VariablePat s -> [s]
        ConstructorPat (a,b) ->a:checkHelper b
        TuplePat ps -> concatMap checkHelper ps


checkDuplicate :: Eq a => [a] -> Bool
checkDuplicate [] = False
checkDuplicate [_] = True
checkDuplicate (x:xs)
    | x `elem` xs = False
    | otherwise = checkDuplicate xs

-- Problem 4
    -- Checks if Tuple of Value and Pattern match
    -- Takes a tuple pair of Value and Pattern
    -- Returns a Maybe List with the String and Value
match :: (Value, Pattern) -> Maybe [(String,Value)]
match (v,p) = case (v,p) of
    (_, WildcardPat) -> Just [("",v)]
    (_,VariablePat s) ->Just [(s,v)]
    (Unit, UnitPat) -> Just[("",v)]
    (Constant x,ConstantPat y) -> if x == y then Just[("",v)] else Nothing
    (Constructor (s2,v2),ConstructorPat (s1,p2)) -> if s1 == s2 then fmap (:) (Just (s1,v)) <*> match (v2,p2) else Nothing
    (Tuple vs, TuplePat ps) -> if length ps == length vs then allAnswers match (mHelper1 vs ps) else Nothing
    _ -> Nothing    

mHelper1 :: [a] -> [b] -> [(a, b)]
mHelper1 _ [] = []
mHelper1 [] _ = []
mHelper1 (v:vs) (p:ps) = (v,p): mHelper1 vs ps



-- Problem 5
    -- Checks the Pattern list if it matches an Value
    -- Takes a Value and Pattern List
    -- Returns Nothing if no pattern matches and Just List tuple pair of String and Value 
firstMatch :: Value -> [Pattern] -> Maybe [(String,Value)]
firstMatch v ps = firstAnswer match (fHelper v ps)

fHelper :: a1 -> [a2] -> [(a1, a2)]
fHelper _ [] = []
fHelper v (p:ps) = (v,p): fHelper v ps