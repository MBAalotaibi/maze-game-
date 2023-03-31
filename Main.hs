module Main (get_maze, print_maze, is_wall, place_player, move, can_move, game_loop, get_path, main) where 

import System.Environment

maze_path = "D:\\haskellAssignment3\\maze2.txt"

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

get_maze maze_path = do 
 mazeContent <- readFile maze_path
 return (lines mazeContent)
-- get_maze = do 
-- x:xs <- readFile maze_path
-- putStrLn (x:xs)


-- Question 2

print_maze :: [String] -> IO ()
print_maze [] = putStrLn ""
print_maze [x] = putStrLn x
print_maze (x:xs) = do
 putStrLn x
 print_maze xs

-- Question 3

is_wall :: [String] -> (Int, Int) -> Bool
is_wall mazeSt (x,y) 
 |(get mazeSt x y) == '#' = True   --using Gaurds 
 |otherwise = False


-- Question 4

place_player :: [String] -> (Int, Int) -> [String]
place_player mazeSt (x,y) = set mazeSt x y '@'


---- Part B

-- Question 5

move :: (Int, Int) -> Char -> (Int, Int)
move (x,y) 'w' =  (x , y-1)
move (x,y) 's' =  (x , y+1)
move (x,y) 'a' =  (x-1 , y)
move (x,y) 'd' =  (x+1 , y)
move (x,y) _ =  (x,y)


-- Question 6

can_move :: [String] -> (Int, Int) -> Char -> Bool
-- can_move mazeSt (x,y) ch
 -- |is_wall mazeSt (move (x,y) ch) == True = False
 -- |otherwise = True
can_move mazeSt (x,y) ch = if is_wall mazeSt (move (x,y) ch) then False else True
-- Question 7

game_loop :: [String] -> (Int, Int) -> IO ()
game_loop mazStr (x,y) = do
 print_maze (place_player mazStr (x,y))
 input <- getLine
 if can_move mazStr (x,y)(input!!0) == True 
 then game_loop mazStr (move (x,y)(input!!0))
 else game_loop mazStr (x,y)


---- Part C

-- Question 8

get_path :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)]

front m start     --this will only work for one maze which is maze2
 |(can_move m start 'd' ) = start : (front m (move start 'd'))
 |(can_move m start 's') = start  : (front m (move start 's'))
 |otherwise = [start]
get_path m start target = front m start



-- Question 9

main :: IO ()

main = error "Not implemented"
