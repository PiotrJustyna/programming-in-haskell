-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "factorial 3"
    putStrLn . show $ factorial 3

    putStrLn "factorial 0"
    putStrLn . show $ factorial 0

    putStrLn "factorial -3"
    putStrLn . show $ factorial (-3)

-- How does the recursive version of the factorial function behave
-- if applied to a negative argument, such as (-1)?
-- Modify the definition to prohibit negative arguments
-- by adding a guard to the recursive case.
factorial :: Int -> Int
factorial 0 = 1
factorial n
    | n < 0 = error "Negative arguments are not expected."
    | otherwise = n * factorial (n-1)