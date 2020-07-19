-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
-- this function converts a character (our set characters of integers 1-9) into its corresponding integer
-- used otherwise in order for code not to break
char_to_int x
    | x == '0' = 0
    | x == '1' = 1
    | x == '2' = 2
    | x == '3' = 3
    | x == '4' = 4
    | x == '5' = 5
    | x == '6' = 6
    | x == '7' = 7
    | x == '8' = 8
    | x == '9' = 9
    | otherwise = 0



repeat_char :: Char -> Integer -> String
-- this function repeats the character or "x", "n" number of times, we need to output this as a string
-- begin with base case which is responsible for when recursion ends (in this case 0)
repeat_char x 0 = ""
repeat_char x n = [x] ++ repeat_char x(n-1)



decode :: String -> String
-- this function decodes the encoded string given to us e.g. "a2b3" returns "aabbb"
-- we begin with a base case if a list with no elements is input
decode [] = []
-- similarly create a base case if only one element is part of the list, hence program doesn't crash
decode [x] = []
decode (x:y:xs) = repeat_char x (char_to_int y) ++ decode xs



-- Part B

-- here we are going to use the code from our answer to question 1 and rearanging it to create our desired outcome
int_to_char :: Integer -> Char
int_to_char x 
    | x == 0 = '0'
    | x == 1 = '1'
    | x == 2 = '2'
    | x == 3 = '3'
    | x == 4 = '4'
    | x == 5 = '5'
    | x == 6 = '6'
    | x == 7 = '7'
    | x == 8 = '8'
    | x == 9 = '9'
    | otherwise = '0'


-- for this task, we are going to create a function which returns how many times a character occurs in the begining of a string
-- in order to do this, we need to count how many times this character is repeated until a different character occurs
length_char :: Char -> String -> Integer
-- first we need to create a base case in order for our function to stop counting the character once list is empty
length_char x [] = 0
length_char x (y:ys)
    | x == y = 1 + length_char x ys
    | otherwise = 0

-- for this task, we need to use similar syntax, which we used for the previous question
-- we are going to remove specific characters from a string
-- in order to do this, we are going to need to build an empty list of characters
-- then we are going to add characters which are not the ones we want to drop, to this empty list
drop_char :: Char -> String -> String
drop_char x [] = []
drop_char x (y:ys)
    | x /= y = (y:ys)
-- doesn't work with == but works with /= so have to rearange the recursion
    | otherwise = drop_char x ys

-- for this task, we need to use similar logic to the one we used for question 3 as this is the reversed outcome
-- similarly, we need to consider all parts of this problem seperately
encode :: String -> String
-- first we need to create a base case which would simply be an empty list
encode [] = []
-- then we need to take the string of characters and find the length of the repeated ones
-- then we need to convert this length(integer) into a character and print it
-- then drop all of the repeated characters and add one of the characters onto the end
encode (x:xs) = x : int_to_char(length_char x (x:xs)) : encode(drop_char (x) (x:xs))




-- Part C

complex_encode :: String -> String
--complex_encode [] = []
--complex_encode (x:xs) = x : int_to_char(length x (x:xs)) : complex_decode(drop_char(x:xs))
complex_encode = error "Not implemented"


complex_decode :: String -> String
complex_decode = error "Not implemented"
