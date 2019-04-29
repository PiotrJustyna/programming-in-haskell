-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter10_exercise4
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
    adder

getNumber :: String -> IO Int
getNumber prompt = do
    putStr prompt
    x <- getLine
    case ((reads x) :: [(Int, String)]) of
        [(number, _)] -> return number
        _ -> do
            putStrLn "Error: not a number."
            getNumber prompt

adder :: IO ()
adder = do
    input <- getNumber "How many numbers?"
    total <- adderHelper 0 input
    putStrLn $ show total

adderHelper :: Int -> Int -> IO Int
adderHelper currentTotal 0 = return currentTotal
adderHelper currentTotal numbersLeftToRead = do
    input <- getNumber ("Number " ++ (show numbersLeftToRead) ++ ": ")
    adderHelper (currentTotal + input) (numbersLeftToRead - 1)