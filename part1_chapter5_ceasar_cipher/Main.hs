-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_ceasar_cipher
-- :load Main
import Data.Char

main = do
    putStrLn "lettterToInt 'a'"
    putStrLn . show $ lettterToInt 'a'

    putStrLn "intToLetter 0"
    putStrLn . show $ intToLetter 0

    putStrLn "shift 3 'a'"
    putStrLn . show $ shift 3 'a'

    putStrLn "shift 3 'z'"
    putStrLn . show $ shift 3 'z'

    putStrLn "shift (-3) 'c'"
    putStrLn . show $ shift (-3) 'c'

    putStrLn "shift 3 ' '"
    putStrLn . show $ shift 3 ' '

    putStrLn "encode 3 \"haskell is fun\""
    putStrLn . show $ encode 3 "haskell is fun"

    putStrLn "encode (-3) \"kdvnhoo lv ixq\""
    putStrLn . show $ encode (-3) "kdvnhoo lv ixq"

lettterToInt :: Char -> Int
lettterToInt x = (ord x) - (ord 'a')

intToLetter :: Int -> Char
intToLetter x = chr (x + (ord 'a'))

shift :: Int -> Char -> Char
shift shiftFactor character
    | isLower character =
        intToLetter $ ((lettterToInt character) + shiftFactor) `mod` 26
    | otherwise = character

encode :: Int -> String -> String
encode shiftFactor text = [shift shiftFactor x | x <- text]