-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise7
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "merge ([] :: [Int]) ([] :: [Int])"
    putStrLn . show $ merge ([] :: [Int]) ([] :: [Int])

    putStrLn "merge [1, 2, 3] ([] :: [Int])"
    putStrLn . show $ merge [1, 2, 3] ([] :: [Int])

    putStrLn "merge ([] :: [Int]) [4, 5, 6]"
    putStrLn . show $ merge ([] :: [Int]) [4, 5, 6]

    putStrLn "merge [2,5,6] [1,3,4]"
    putStrLn . show $ merge [2,5,6] [1,3,4]

-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a]
-- that merges two sorted lists to give a single sorted list.
-- For example:
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys) 
