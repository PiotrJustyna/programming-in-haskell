-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise6
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "and' [True, False, False]"
    putStrLn . show $ and' [True, False, False]

    putStrLn "concat' [[1, 2], [3, 4], [5]]"
    putStrLn . show $ concat' [[1, 2], [3, 4], [5]]

    putStrLn "concat' ([[]] :: [[Int]])"
    putStrLn . show $ concat' ([[]] :: [[Int]])

    putStrLn "concat' ([] :: [[Int]])"
    putStrLn . show $ concat' ([] :: [[Int]])

    putStrLn "replicate' 3 \"a\""
    putStrLn . show $ replicate' 3 "a"

    putStrLn "replicate' 0 \"a\""
    putStrLn . show $ replicate' 0 "a"

    -- This causes an error (purposefully), so I commented it out.
    -- putStrLn "replicate' (-3) \"a\""
    -- putStrLn . show $ replicate' (-3) "a"

    putStrLn "nth [1, 2, 3] 1"
    putStrLn . show $ nth [1, 2, 3] 1

    -- This causes an error (purposefully), so I commented it out.
    -- putStrLn "nth ([] :: [Int]) 1"
    -- putStrLn . show $ nth ([] :: [Int]) 1

    -- This causes an error (purposefully), so I commented it out.
    -- putStrLn "nth [1, 2, 3] (-1)"
    -- putStrLn . show $ nth [1, 2, 3] (-1)

    -- This causes an error (purposefully), so I commented it out.
    -- putStrLn "nth [1, 2, 3] 3"
    -- putStrLn . show $ nth [1, 2, 3] 3

    putStrLn "isElementOf 1 [1, 2, 3]"
    putStrLn . show $ isElementOf 1 [1, 2, 3]

    putStrLn "isElementOf 4 [1, 2, 3]"
    putStrLn . show $ isElementOf 4 [1, 2, 3]

    putStrLn "isElementOf 4 []"
    putStrLn . show $ isElementOf 4 []

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | x == True = and' xs
    | otherwise = False

concat' :: Show a => [[a]] -> [a]
concat' [] = []
concat' (x:[]) = x
concat' (x:(y:ys)) = concat' ((x ++ y):ys)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x y
    | x < 0 = error "Negative number of replicas is not expected."
    | otherwise = y : (replicate' (x - 1) y)

nth :: [a] -> Int -> a
nth [] _ = error "Cannot index an empty list."
nth (x:xs) y
    | y < 0 = error "Negative index is not expected."
    | y >= length (x:xs) = error "Index is outside of the list range."
    | y == 0 = x
    | otherwise = nth xs (y - 1)


isElementOf :: Eq a => a -> [a] -> Bool
isElementOf _ [] = False
isElementOf x (y:ys)
    | x == y = True
    | otherwise = isElementOf x ys