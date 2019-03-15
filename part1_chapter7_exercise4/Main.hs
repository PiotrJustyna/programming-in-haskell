-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise4
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "dec2int [2, 3, 4, 5]"
    putStrLn . show $ dec2int [2, 3, 4, 5]

dec2int :: [Int] -> Int
dec2int = foldl (\x y -> (10 * x) + y) 0