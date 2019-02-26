-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_ceasar_cipher
-- option 2 (stack):
-- stack ghci
--
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

    putStrLn "percent 5 15"
    putStrLn . show $ percent 5 15

    putStrLn "numberOfLowercaseCharacters \"Haskell\""
    putStrLn . show $ numberOfLowercaseCharacters "Haskell"

    putStrLn "count 'l' \"Haskell\""
    putStrLn . show $ count 'l' "Hasell"

    putStrLn "frequencies \"Haskell\""
    putStrLn . show $ frequencies "Haskell"

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

percent :: Int -> Int -> Float
percent x y = ((fromIntegral x) / (fromIntegral y)) * 100

numberOfLowercaseCharacters :: String -> Int
numberOfLowercaseCharacters text = length $ filter isLower text

count :: Char -> String -> Int
count characterToCount text = length $ filter (\x -> x == characterToCount) text

frequencies :: String -> [(Char, Float)]
frequencies text =
    [(x, percent (count x text) (numberOfLowercaseCharacters text)) | x <- ['a' .. 'z']]