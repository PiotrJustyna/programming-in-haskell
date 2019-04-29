-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter10_exercise6
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char
import System.IO

main = do
    line <- getLine
    putStrLn line

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    x <- getChar
    hSetEcho stdin True
    return x

-- I'm either misunderstanding the challenge,
-- or the Delete key simply works out of the box.
getLine' :: IO String
getLine' = do
    character <- getCh
    putStrLn . show $ character
    return $ show character