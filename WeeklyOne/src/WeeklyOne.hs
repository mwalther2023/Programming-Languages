
module WeeklyOne where

--Problem 1
    --This function removes all occurences of the given char from a string
    --It takes a cahr parameter and a string
    --This funciton returns a string with all occurences of the given char removed
removeChar c "" = ""
removeChar c (x:xs) = if c == x
                            then removeChar c xs
                        else x : removeChar c xs

--Problem 2
    --This function removes all whitespace in a string
    --It takes a string as the only parameter
    --This function returns the string with all whitespace removed
removeWhitespace xs = removeChar ' '
                        (removeChar '\t'
                            (removeChar '\n'
                                (removeChar '\r' xs)))

--Problem 4
    --This function removes all punctuation in a given string
    --It takes a string as the only parameter
    --It returns a string with all punctuation removed
removePunctuation xs =  removeChar ','
                        (removeChar '.'
                            (removeChar '('
                            (removeChar ')'
                                    (removeChar '['
                                    (removeChar ']'
                                        (removeChar '{'
                                        (removeChar '}' xs)))))))

--Problem 5
    --This function converts all chars in a given string into their ASCII value
    --This function takes a string as the only parameter
    --This function returns a list of Integer values representing ASCII values
charsToAscii [] = []
charsToAscii (x:xs) = fromEnum x : charsToAscii xs

--Problem 6
    --This function converts a list of Integers representing ASCII values into their respective char 
    --It takes a list of Integers
    --This function returns a string
asciiToChars [] = []
asciiToChars (x:xs) = char x : asciiToChars xs
char :: Int -> Char
char x = toEnum x::Char

--Problem 7
    --This function shifts each Integer value by the given Integer
    --This function takes an Interger and a list of Integers
    --It returns a list of Integers
shiftInts i [] = []
shiftInts i (x:xs) = (if x+i < 128 then x+i else (x+i-128)) : shiftInts i xs


--Problem 8
    --This function shifts the ASCII value of each char in the given string by the given Integer
    --It takes an Integer and a string as parameters
    --It returns a string where all chars had their ASCII value changed
shiftMessage i [] = []
shiftMessage i (x:xs) = shifter2 x i : shiftMessage i xs
shifter2 x i = toEnum((fromEnum x)+i)::Char