-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "2 ^^^ 3"
    putStrLn $ show (2 ^^^ 3)

    putStrLn "2 ^^^ 0"
    putStrLn $ show (2 ^^^ 0)

    putStrLn "2 ^^^ (-1)"
    putStrLn $ show (2 ^^^ (-1))

-- Define the exponentiation operator ^ for non-negative integers
-- using the same pattern of recursion as the multiplication operator *,
-- and show how the expression 2 ^ 3 is evaluated using your definition.
infixl 5 ^^^
(^^^) :: Int -> Int -> Int
(^^^) 0 0 = error "Undefined."
(^^^) _ 0 = 1
(^^^) x y
    | x < 0 ||  y < 0 = error "Negative integers are not expected."
    | otherwise = x * (x ^^^ (y - 1))
