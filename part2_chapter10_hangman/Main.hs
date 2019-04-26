-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter10_hangman
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import System.IO

main = hangman

hangman :: IO ()
hangman = do
    putStrLn "Think of a word:"
    word <- customGetLine
    putStrLn "Now try to guess it:"
    play word

customGetLine :: IO String
customGetLine = do
    x <- customGetChar
    if (x == '\n')
        then
            do
                putChar x
                return ""
        else
            do
                putChar '-'
                xs <- customGetLine
                return (x:xs)

customGetChar :: IO Char
customGetChar = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

play :: String -> IO ()
play word = do
    putStrLn "? "
    guess <- customGetLine
    if (guess == word)
        then
            putStrLn "You got it."
        else
            do
                putStrLn (match word guess)
                play word

match :: String -> String -> String
match xs ys = [if (elem y xs) then y else '-' | y <- ys]