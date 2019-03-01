-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise9
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "scalarProduct [1, 2, 3] [4, 5, 6]"
    putStrLn . show $ scalarProduct [1, 2, 3] [4, 5, 6]

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ (xs !! index) * (ys !! index) | index <- [0 .. ((length xs) - 1)]]