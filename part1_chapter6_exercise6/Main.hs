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

    putStrLn "concat' [[]]"
    putStrLn . show $ concat' ([[]] :: [[Int]])

    putStrLn "concat' []"
    putStrLn . show $ concat' ([] :: [[Int]])

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs)
    | x == True = and' xs
    | otherwise = False

concat' :: Show a => [[a]] -> [a]
concat' [] = []
concat' (x:[]) = x
concat' (x:(y:ys)) = concat' ((x ++ y):ys)