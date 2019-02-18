-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter3_exercise2
-- :load Main
main = do
    putStrLn "bools"
    putStrLn . show $ bools

    putStrLn "nums"
    putStrLn . show $ nums

    putStrLn "add 1 2 3"
    putStrLn . show $ add 1 2 3

    putStrLn "copy \"z\""
    putStrLn . show $ copy "z"

    putStrLn "apply (+2) 3"
    putStrLn . show $ apply (+2) 3

----------------------------------------

bools :: [Bool]
bools = [True, False]

----------------------------------------

nums :: [[Int]]
nums = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

----------------------------------------

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

----------------------------------------

copy :: a -> (a,a)
copy x = (x, x)

----------------------------------------

apply :: (a -> b) -> a -> b
apply f x = f x

----------------------------------------