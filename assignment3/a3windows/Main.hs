module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 
import System.IO
import System.Environment

maze_path = "C:\\assignment3\\a3windows\\maze2.txt"

-- Useful code from Lecture 25
-- You may use this freely in your solutions

get :: [String] -> Int -> Int -> Char
get maze x y = (maze !! y) !! x 

modify_list :: [a] -> Int -> a -> [a]
modify_list list pos new =
    let
        before = take  pos    list
        after  = drop (pos+1) list
    in
        before ++ [new] ++ after

set :: [String] -> Int -> Int -> Char -> [String]
set maze x y char = 
    let
        line = maze !! y
        new_line = modify_list line x char
        new_maze = modify_list maze y new_line
    in
        new_maze


---- Part A

-- Question 1

get_maze :: String -> IO [String]
-- Here we need to write a function, which takes the file path and returns the maze as a list of strings
get_maze file = do
-- Here need to unbox the file
    x <- readFile file
    let
        formatted = lines x
    return formatted

-- Question 2

print_maze :: [String] -> IO ()
-- Here we just need to use the putStrLn command and the unlines command, which we have seen in previous lectures
print_maze maze = putStrLn (unlines maze)

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
-- For this function, we need to implement simple hit detection based on coordinates
-- if the coordinates of x y are equal to a "#", then a wall is detected
is_wall maze (x,y) = if get maze x y == '#' then 
                     True else
                          False

-- Question 4

-- For this task, we are simply going to create a player and place an '@' character at the coordinates of said player
place_player :: [String] -> (Int, Int) -> [String]
place_player maze (x,y) = set maze x y '@'


---- Part B

-- Question 5

--For this task, we will need to simply change (or not change) the coordinates based on the input
move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) 'w' = (x,(y-1))
move (x,y) 's' = (x,(y+1))
move (x,y) 'a' = ((x-1),y)
move (x,y) 'd' = ((x+1),y)
move (x,y)  _  = (x,y)

-- Question 6

--For this function we will take the coordinates of a player and detect whether they can move in any direction
--this should return a true or false value based on the coordinates of nearby walls
can_move :: [String] -> (Int, Int) -> Char -> Bool
can_move maze (x,y) dirChr = let 
                        moves = (move (x,y) dirChr)
                            in 
                        if is_wall maze moves == True then False else True
                        
-- Question 7

--For this IO action, we will implement the maze game
--this will require for us to print the maze with player position and move the player based on input
--we should also update this every time a user inputs a key
--if the player can move, we will update coordinates (if not then don't change anything)
game_loop :: [String] -> (Int, Int) -> IO ()
--we will also need to recursively call the function to loop the game
game_loop maze (x,y) = do
                print_maze $ place_player maze (x,y)
                input <- getLine
                let dirChr = input !! 0
                if can_move maze (x,y) dirChr 
                then game_loop maze (move (x,y) dirChr) 
                else game_loop maze (x,y)



---- Part C

-- Question 8

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
get_path = error "Not implemented"

-- Question 9

main :: IO ()
main = error "Not implemented"
