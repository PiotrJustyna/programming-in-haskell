-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise7
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn "luhnDouble 3"
    putStrLn . show $ luhnDouble 3

    putStrLn "luhnDouble 6"
    putStrLn . show $ luhnDouble 6

    putStrLn "luhn 1 7 8 4"
    putStrLn . show $ luhn 1 7 8 4

    putStrLn "luhn 4 7 8 3"
    putStrLn . show $ luhn 4 7 8 3

-- The Luhn algorithm is used to check bank card numbers
-- for simple errors such as mistyping a digit, and proceeds as follows:
-- * consider each digit as a separate number;
-- * moving left, double every other number from the second last;
-- * subtract 9 from each number that is now greater than 9;
-- * add all the resulting numbers together;
-- * if the total is divisible by 10, the card number is valid.
-- Define a function luhnDouble :: Int -> Int that doubles a digit
-- and subtracts 9 if the result is greater than 9.
-- For example:
-- > luhnDouble 3
-- 6
-- > luhnDouble 6
-- 3
-- Using luhnDouble and the integer remainder function mod,
-- define a function luhn :: Int -> Int -> Int -> Int -> Bool
-- that decides if a four-digit bank card number is valid.
-- For example:
-- > luhn 1 7 8 4
-- True
-- > luhn 4 7 8 3
-- False
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z
    | mod luhnSum 10 == 0 = True
    | otherwise = False
    where
        luhnSum = (luhnDouble w) + x + (luhnDouble y) + z

luhnDouble :: Int -> Int
luhnDouble x
    | double > 9 = double - 9
    | otherwise = double
    where
        double = x * 2