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
    putStrLn "putGrid sampleGrid1:"
    putGrid sampleGrid1

size :: Int
size = 3

data Player = O | Blank | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

sampleGrid1 :: Grid
sampleGrid1 = [[Blank, O, O], [O, X, O], [X, X, X]]

next :: Player -> Player
next O = X
next X = O
next Blank = error "Argument does not apply in the context of \"next\""

empty :: Grid
empty = replicate size (replicate size Blank)

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