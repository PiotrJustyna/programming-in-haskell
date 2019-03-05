-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise8
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.List

main = do
    putStrLn "positions 1 [1, 2, 3, 1]"
    putStrLn . show $ positions 1 [1, 2, 3, 1]

    putStrLn "position' 1 [1, 2, 3, 1]"
    putStrLn . show $ positions' 1 [1, 2, 3, 1]

    putStrLn "position'' 1 [1, 2, 3, 1]"
    putStrLn . show $ positions'' 1 [1, 2, 3, 1]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

-- no zip (not sure why I did that)
positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = [index | index <- [0 .. ((length xs) - 1)], (xs !! index) == x]

-- using find
positions'' :: Eq a => a -> [a] -> [Int]
positions'' x xs = [index | index <- [0 .. ((length xs) - 1)], find (\y -> y == (xs !! index)) xs == Just x]