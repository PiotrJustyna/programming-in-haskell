-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise9
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "sum' ([] :: [Int])"
    putStrLn . show $ sum' ([] :: [Int])

    putStrLn "sum' [1, 2, 3]"
    putStrLn . show $ sum' [1, 2, 3]

    putStrLn "take' 34 ([] :: [Int])"
    putStrLn . show $ take' 34 ([] :: [Int])

    putStrLn "take' (-1) [1, 2, 3]"
    putStrLn . show $ take' (-1) [1, 2, 3]

    putStrLn "take' 0 [1, 2, 3]"
    putStrLn . show $ take' 0 [1, 2, 3]

    putStrLn "take' 2 [1, 2, 3]"
    putStrLn . show $ take' 2 [1, 2, 3]

    putStrLn "take' 4 [1, 2, 3]"
    putStrLn . show $ take' 4 [1, 2, 3]

    -- This causes an error (purposefully), so I commented it out.
    -- putStrLn "last' ([] :: [Int])"
    -- putStrLn . show $ last' ([] :: [Int])

    putStrLn "last' [1]"
    putStrLn . show $ last' [1]

    putStrLn "last' [1, 2, 3]"
    putStrLn . show $ last' [1, 2, 3]

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + (sum' xs)

take' :: Show a => Int -> [a] -> [a]
take' _ [] = []
take' x (y:ys)
    | x <= 0 = []
    | otherwise = y : (take' (x - 1) ys)

last' :: Show a => [a] -> a
last' [] = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs