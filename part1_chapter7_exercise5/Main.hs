-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise5
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "curry' uncurried 3 4"
    putStrLn .show $ curry' uncurried 3 4

    putStrLn "uncurry' curried (3, 4)"
    putStrLn .show $ uncurry' curried (3, 4)

uncurried :: (Int, Int) -> Int
uncurried = \(x, y) -> x + y

curried :: Int -> Int -> Int
curried = \x y -> x - y

-- I had absolutely no idea what the author had in mind,
-- so very happy the below was my first guess :D
-- curry :: (a, b) -> c -> (a -> b -> c)
-- The actual signature is taken from the solutions sections, though.
-- I had to make sure what the exercise really was.
curry' :: (Num a, Num b, Num c) => ((a, b) -> c) -> (a -> b -> c)
curry' x = \y z -> x (y, z)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' x = \(y, z) -> x y z