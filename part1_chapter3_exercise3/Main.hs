-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter3_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "second [1, 2, 3]"
    putStrLn . show $ second [1, 2, 3]

    putStrLn "swap (1, 2)"
    putStrLn . show $ swap (1, 2)

    putStrLn "swap (1, \"2\")"
    putStrLn . show $ swap (1, "2")

    putStrLn "pair 1 \"2\""
    putStrLn . show $ pair 1 "2"

    putStrLn "double 1"
    putStrLn . show $ double 1

    putStrLn "palindrome [1, 2, 3, 4, 3, 2, 1]"
    putStrLn . show $ palindrome [1, 2, 3, 4, 3, 2, 1]

    putStrLn "twice twice (\\x -> x + 1) 1"
    putStrLn . show $ twice (\x -> x + 1) 1

----------------------------------------

second :: Show a => [a] -> a
second xs = head (tail xs)

----------------------------------------

swap :: (Show a, Show b) => (a, b) -> (b, a)
swap (x,y) = (y,x)

----------------------------------------

pair :: (Show a, Show b) => a -> b -> (a, b)
pair x y = (x,y)

----------------------------------------

double :: Num a => a -> a
double x = x*2

----------------------------------------

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

----------------------------------------

twice :: (a -> a) -> a -> a
twice f x = f (f x)

----------------------------------------