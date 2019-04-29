-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter10_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "putStr test:"
    putStr "test"
    putStrLn ""

    putStrLn "putStr' test:"
    putStr' "test"
    putStrLn ""

putStr' :: String -> IO ()
putStr' text = sequence_ [
    do
        putChar singleCharacter |
    singleCharacter <- text]