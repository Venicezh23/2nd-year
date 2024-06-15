-- Tic-tac-toe example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Basic declarations

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities

empty :: Grid 
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p) --predicate - checks if all elements in list are == to player p
              rows = g --list of rows in grid
              cols = transpose g --turns rows to cols
              dias = [diag g, diag (map reverse g)] --checking main dia and reverse dia

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]
--gets main diagonal
--get element at positions (n,n) from 0 to size-1

won :: Grid -> Bool
won g = wins O g || wins X g

-- Displaying a grid

putGrid :: Grid -> IO ()
putGrid = --accepts a grid (in the beginning, it's empty -> 9 B's)
   putStrLn . unlines . concat . interleave bar . map showRow
   where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
--1. converts B into empty spaces in showPlayer (create small row for large row)
--2. interleave takes empty spaces and | x3 - creates n rows of size n
--3. combine the strings horizontally using foldrl 
showRow = beside . interleave bar . map showPlayer --gets empty B's for mapping
          where
             beside = foldr1 (zipWith (++))
             bar    = replicate 3 "|" --to separate spaces as O or X goes in center

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "] --starting point of empty grid (1 small row of overall row in 3x3 grid)
--outcome of B + interleave = [   |   |   ]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys 
--outcome of interleave for beginning empty grid: 
{-
   |   |
   |   |
   |   |
-}

-- Making a move

--checks grid and input
--if input is >= 0 and i < size^2 (max input for a grid size n*n)
--and check if element at index i in flattened grid g is == B aka EMPTY
valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B
--example of concat: concat [[1,2,3,1,2,3]] !! 0 == 1 ==> True
--note: element at index 0 is 1 -> True

move:: Grid -> Int -> Player -> [Grid]
move g i p =
    --if valid input, chop list into 2 parts at index i, else return empty list
   if valid g i then [chop size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)
   --flatten grid g, then splitAt i to create xs and ys
   --B to represent element at index i

--function to split list at index i
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a natural number

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

-- Human vs human

tictactoe :: IO ()
tictactoe = run empty O 

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1,1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g  = putStrLn "Player O wins!\n"
         | wins X g  = putStrLn "Player X wins!\n"
         | full g    = putStrLn "It's a draw!\n"
         | otherwise =
              do i <- getNat (prompt p)
                 case move g i p of
                    []   -> do putStrLn "ERROR: Invalid move"
                               run' g p
                    [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J" --ANSI escape character - instructs terminal to clear screen

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")
--ANSI escape character - move cursor to specific position (row, column)

-- Game trees

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- Minimax

minimax :: Tree Grid -> Tree (Grid,Player)
minimax (Node g [])
   | wins O g  = Node (g,O) []
   | wins X g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax (Node g ts) 
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree

-- Human vs computer

main :: IO ()
main = do hSetBuffering stdout NoBuffering --sets buffering for standard output - output flushed immediately for user experience
          play empty O --calls function play and inserts empty (replicated B's according to size) and player O

play :: Grid -> Player -> IO ()
play g p = do cls --clean screen
              goto (1,1) --points cursor to coordinate 1,1
              putGrid g --prepare tic tac toe grid (starting from empty)
              play' g p --calls play' function and inserts grid and player O

play' :: Grid -> Player -> IO ()
play' g p
   | wins O g = putStrLn "Player O wins!\n"
   | wins X g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g p --if return empty list - use play'
                      [g'] -> play g' (next p) --if return non empty list - use play
                        --updates game grid and switches to next player
                        --if current Player O, the next O = X
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove g p)) (next p)
                   -- $! ensures result of (bestmove g p) is fully evaluated
                   -- used to strict func application
                   --forcefully evaluates expression on right before left
