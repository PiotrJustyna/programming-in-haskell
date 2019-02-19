-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise1
-- :load Main
main = do
    putStrLn "halve [1, 2, 3, 4, 5, 6]"
    putStrLn . show $ halve [1, 2, 3, 4, 5, 6]

    putStrLn "halve [1, 2, 3]"
    putStrLn . show $ halve [1, 2, 3]

    putStrLn "thirdHeadAndTail [1, 2, 3, 4]"
    putStrLn . show $ thirdHeadAndTail [1, 2, 3, 4]

    putStrLn "thirdListIndexing [1, 2, 3, 4]"
    putStrLn . show $ thirdListIndexing [1, 2, 3, 4]

    putStrLn "thirdPatternMatching [1, 2, 3, 4]"
    putStrLn . show $ thirdPatternMatching [1, 2, 3, 4]
    
-- Using library functions, define a function halve :: [a] -> ([a],[a])
-- that splits an even-lengthed list into two halves.
-- For example:
-- > halve [1, 2, 3, 4, 5, 6]
-- ([1, 2, 3], [4, 5, 6])
halve :: Show a => [a] -> ([a], [a])
halve xs
    | (length xs) `mod` 2 == 0 = (take half xs, drop half xs)
    | otherwise = (xs, [])
    where half = ((length xs) `div` 2)

-- Define a function third :: [a] -> a
-- that returns the third element in a list
-- that contains at least this many elements using:
-- a.​head and tail;
-- b.​list indexing !!;
-- c.​pattern matching.
thirdHeadAndTail :: [a] -> a
thirdHeadAndTail xs = head . tail $ tail xs

thirdListIndexing :: [a] -> a
thirdListIndexing xs = xs !! 2

thirdPatternMatching :: [a] -> a
thirdPatternMatching (x0 : x1 : x2 : xs) = x2
