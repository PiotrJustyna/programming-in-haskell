-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_ceasar_cipher
-- option 2 (stack):
-- stack ghci
--
-- :load Main
import Data.Char
import Data.List
import Data.Maybe

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

    putStrLn "chiSquared [2.0 3.0 4.0] [3.0 4.0 5.0]"
    putStrLn . show $ chiSquared [2.0, 3.0, 4.0] [1.0, 1.0, 1.0]

    putStrLn "rotate 1 \"Haskell\""
    putStrLn . show $ rotate 1 "Haskell"

    putStrLn "crackCeasarCipher \"kdvnhoo lv ixq\""
    putStrLn . show $ crackCeasarCipher "kdvnhoo lv ixq"

    putStrLn "crackCeasarCipher \"vscd mywzboroxcsyxc kbo ecopev\""
    putStrLn . show $ crackCeasarCipher "vscd mywzboroxcsyxc kbo ecopev"

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

frequencies :: String -> [Float]
frequencies text =
    [percent (count x text) (numberOfLowercaseCharacters text) | x <- ['a' .. 'z']]

chiSquared :: [Float] -> [Float] -> Float
chiSquared observedFrequencies expectedFrequencies =
    sum [((observedFrequency - expectedFrequency) ^ 2) / expectedFrequency | (observedFrequency, expectedFrequency) <- zip observedFrequencies expectedFrequencies]

rotate :: Show a => Int -> [a] -> [a]
rotate rotationFactor xs = (drop rotationFactor xs) ++ (take rotationFactor xs)

-- Character frequencies of a large volume of text.
table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crackCeasarCipher :: String -> String
crackCeasarCipher text = encode (-factor) text
    where
        factor = fromMaybe 0 (elemIndex (minimum chiSquaredResults) chiSquaredResults)
        chiSquaredResults = [chiSquared (rotate n (frequencies text)) table | n <- [0 .. 25]]
