-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "sumdown 3"
    putStrLn . show $ sumdown 3

    putStrLn "sumdown (-3)"
    putStrLn . show $ sumdown (-3)

-- Define a recursive function sumdown :: Int -> Int
-- that returns the sum of the non-negative integers
-- from a given value down to zero.
-- For example, sumdown 3 should return the result 3+2+1+0 = 6.
sumdown :: Int -> Int
sumdown 0 = 0
sumdown x
    | x < 0 = error "Negative arguments are not expected."
    | otherwise = x + (sumdown (x - 1))