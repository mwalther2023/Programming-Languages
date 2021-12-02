module DailyEight where

-- data Event = Event String Int String Int Double Double deriving(Show)
data Event = Event {name :: String, day :: Int, month :: String, year :: Int, xlocation :: Double, ylocation :: Double} 
    deriving(Show,Eq)

test = Event {name="New Year", day=1, month="Jan", year=2000, xlocation=150, ylocation=150}
test2 = Event {name="Party", day=17, month="Mar", year=2001, xlocation=120, ylocation=120}
test3 = Event {name="Flowers", day=10, month="May", year=2000, xlocation=100, ylocation=100}

list = [test, test2, test3]
-- Problem 1
    -- To create a list of events that have the same year
    -- Takes a year and a list of events
    -- Returns a list of events with the same year
inYear :: Int -> [Event] -> [Event]
inYear x ys = filter (\y -> year y == x) ys


-- Problem 2
    -- Create a list of names of events that occured in the given range of days
    -- It takes a start day and end day with a list of events
    -- Returns a list of event names that occured in the given range of days
inDayRange :: Int ->  Int -> [Event] -> [String]
inDayRange st end xs = (map (name) (filter (\y -> day y >= st && day y <= end) xs))


-- Problem 3
    -- Create a list of events based on the names and area the event took place
    -- Takes a name of event and upper and lower bounds for x and y and a list of events
    -- Returns a list of events that occurred in the given x and y space that have the same name
inArea :: String -> Double -> Double -> Double -> Double -> [Event] -> [Event]
inArea n lowX upX lowY upY xs = filter (\d -> (inX (xlocation d) lowX upX) &&  (inY (ylocation d) lowY upY) && name d == n)xs

inX x lowX upX = x >= lowX && x <= upX
inY y lowY upY = y >= lowY && y <= upY