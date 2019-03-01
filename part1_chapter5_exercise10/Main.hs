-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise10
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char

main = do
    putStrLn "encode 3 \"Haskell\""
    putStrLn . show $ encode 3 "Haskell"

    putStrLn "encode (-3) \"kdvnhoo\""
    putStrLn . show $ encode (-3) "kdvnhoo"

lettterToInt :: Char -> Int
lettterToInt x = (ord x) - (ord 'a')

intToLetter :: Int -> Char
intToLetter x = chr (x + (ord 'a'))

shift :: Int -> Char -> Char
shift shiftFactor character =
    intToLetter $ ((lettterToInt (toLower character)) + shiftFactor) `mod` 26

encode :: Int -> String -> String
encode shiftFactor text = [shift shiftFactor x | x <- text]