module DailyOne where
    -- Matthew Walther
    -- Daily One Homework
--Problem 1:
    --Prints the result of a quadratic equation from the given numbers
        -- in the format a+bx+cx^2
    --The functions parameters are 4 integer values
quadratic a b c x = a+b*x+c*square x

--Problem 2
    --This function scales the given ordered pair by a given integer
    --It takes 2 parameters, an integer and a 2 integer tuple
    --The result is a tuple where each value was multiplied by the single integer
scaleVector n (x,y) = (n*x,n*y)

--Problem 3
    --This function finds the cartesian distance betweed 2 3D ordered pairs
    --It takeks 2 parameters of tuples which hold 3 integers
    --The result is a floating point number which is the cartesian distance between the tuples
tripleDistance (x1,y1,z1) (x2,y2,z2) = sqrt(square(x2-x1)
                                     + square(y2-y1)
                                     + square(z2-z1))

square n = n*n