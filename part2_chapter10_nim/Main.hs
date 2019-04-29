-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter10_nim
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char
import System.IO

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    putStrLn "initial:"
    putStrLn . show $ initial

    putStrLn "isValid initial 1 3:"
    putStrLn . show $ isValid initial 1 3

    putStrLn "isValid initial 4 3:"
    putStrLn . show $ isValid initial 4 3

    putStrLn "move initial 1 3:"
    putStrLn . show $ move initial 1 3

    putStrLn "putRow 1 5:"
    putRow 1 5

    putStrLn "putBoard initial"
    putBoard initial

    play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

isValid :: Board -> Int -> Int -> Bool
isValid board row numberOfStarsToBeRemoved =
    board !! (row - 1) >= numberOfStarsToBeRemoved

move :: Board -> Int -> Int -> Board
move board row numberOfStarsToBeRemoved =
    if (isValid board row numberOfStarsToBeRemoved)
        then
            [if (singleRowIndex == (row - 1))
                then
                    (board !! singleRowIndex) - numberOfStarsToBeRemoved
                else
                    (board !! singleRowIndex) |
                singleRowIndex <- [0 .. ((length board) - 1)]]
        else
            board

putRow :: Int -> Int -> IO ()
putRow row numberOfStars = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate numberOfStars "* "))

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow index value | (index, value) <- zip [1 ..] board]

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    x <- getChar
    newline
    if (isDigit x)
        then
            return (digitToInt x)
        else
            do
                putStrLn "ERROR: Invalid digit."
                getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
    newline
    putBoard board
    if (finished board)
        then
            do
                newline
                putStr "Player "
                putStr (show (next player))
                putStr " wins!"
        else
            do
                newline
                putStr "Player "
                putStrLn (show player)
                row <- getDigit "Row to use: "
                numberOfStars <- getDigit "Number of stars to remove: "
                if (isValid board row numberOfStars)
                    then
                        play (move board row numberOfStars) (next player)
                    else
                        do
                            newline
                            putStrLn "ERROR: invalid move"
                            play board player