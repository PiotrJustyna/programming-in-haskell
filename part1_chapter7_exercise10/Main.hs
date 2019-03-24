-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise10
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char

type Bit = Int

main = do
    putStrLn "luhn [1, 7, 8, 4]"
    putStrLn . show $ luhn [1, 7, 8, 4]

    putStrLn "luhn [4, 7, 8, 3]"
    putStrLn . show $ luhn [4, 7, 8, 3]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ (x:[]) = [f x]
altMap f g (x1:x2:xs) = (f x1) : (g x2) : (altMap f g xs)

luhn :: [Int] -> Bool
luhn xs
    | (sum $ altMap luhnDouble id xs) `mod` 10 == 0 = True
    | otherwise = False

luhnDouble :: Int -> Int
luhnDouble x
    | double > 9 = double - 9
    | otherwise = double
    where
        double = x * 2