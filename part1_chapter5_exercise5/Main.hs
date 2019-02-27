-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise5
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn "pythagoreanIntegers 10"
    putStrLn . show $ pythagoreanIntegers 10

pythagoreanIntegers :: Int -> [(Int, Int, Int)]
pythagoreanIntegers limit = [(x, y, z) | x <- [1 .. limit], y <- [1 .. limit], z <- [1 .. limit], (x ^ 2) + (y ^ 2) == z ^ 2]