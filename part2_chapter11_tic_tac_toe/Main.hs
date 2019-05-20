-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter11_tic_tac_toe
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char
import Data.List
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    tictactoe

size :: Int
size = 3

data Player = O | Blank | X deriving (Eq, Ord, Show)

data Tree a = Node a [Tree a] deriving Show

type Grid = [[Player]]

type Position = (Int, Int)

sampleGrid1 :: Grid
sampleGrid1 = [[Blank, O, O], [O, X, O], [X, X, X]]

next :: Player -> Player
next O = X
next X = O
next Blank = error "Argument does not apply in the context of \"next\""

empty :: Grid
empty = replicate size (replicate size Blank)

depth :: Int
depth = 9

isFull :: Grid -> Bool
isFull = all (/= Blank) . concat

turn :: Grid -> Player
turn grid =
    if numberOfOs <= numberOfXs
        then O
        else X
    where
        numberOfOs = length (filter (== O) gridSpaces)
        numberOfXs = length (filter (== X) gridSpaces)
        gridSpaces = concat grid

doesWin :: Player -> Grid -> Bool
doesWin player grid = any line (rows ++ columns ++ dias)
    where
        line = all (== player)
        rows = grid
        columns = transpose grid
        dias = [diagonal grid, diagonal (map reverse grid)]

diagonal :: Grid -> [Player]
diagonal grid = [grid !! index !! index | index <- [0 .. (size - 1)]]

won :: Grid -> Bool
won grid = (doesWin O grid) || (doesWin X grid)

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
    where
        bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " 0 ", "   "]
showPlayer Blank = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : (interleave x ys)

isValid :: Grid -> Int -> Bool
isValid grid positionIndex =
    (positionIndex >= 0) &&
    (positionIndex < (size ^ 2)) &&
    (concat grid !! positionIndex == Blank)

move :: Grid -> Int -> Player -> [Grid]
move grid positionIndex player =
    if (isValid grid positionIndex)
        then
            [chop size (xs ++ (player:ys))]
        else
            []
    where
        (xs, Blank:ys) = splitAt positionIndex (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : (chop n (drop n xs))

getNaturalNumber :: String -> IO Int
getNaturalNumber prompt =
    do
        putStr prompt
        xs <- getLine
        if (xs /= [] && all isDigit xs)
            then
                return (read xs)
            else
                do
                    putStrLn "Error: invalid number."
                    getNaturalNumber prompt

tictactoe :: IO ()
tictactoe = run empty O

clear :: IO ()
clear = putStr "\ESC[2J"

goto :: Position -> IO ()
goto (x, y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")

run :: Grid -> Player -> IO ()
run grid player = do
    clear
    goto (1, 1)
    putGrid grid
    run' grid player

run' :: Grid -> Player -> IO ()
run' grid player
    | doesWin O grid    = putStrLn $ "Player " ++ (show O) ++ " wins."
    | doesWin X grid    = putStrLn $ "Player " ++ (show X) ++ " wins."
    | isFull grid       = putStrLn $ "It's a draw."
    | otherwise =
        do  i <- getNaturalNumber (prompt player)
            case (move grid i player) of
                [] -> do
                    putStrLn "Error: invalid move."
                    run' grid player
                [newGrid] -> run newGrid (next player)

prompt :: Player -> String
prompt player = "Player " ++ (show player) ++ ", enter your move:"

gameTree :: Grid -> Player -> Tree Grid
gameTree grid player = Node grid [gameTree newGrid (next player) | newGrid <- moves grid player]

moves :: Grid -> Player -> [Grid]
moves grid player
    | won grid = []
    | isFull grid = []
    | otherwise = concat [move grid index player | index <- [0 .. ((size ^ 2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]
