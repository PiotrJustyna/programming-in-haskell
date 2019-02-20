-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise2
-- :load Main
main = do
    putStrLn "thirdHeadAndTail [1, 2, 3, 4]"
    putStrLn . show $ thirdHeadAndTail [1, 2, 3, 4]

    putStrLn "thirdListIndexing [1, 2, 3, 4]"
    putStrLn . show $ thirdListIndexing [1, 2, 3, 4]

    putStrLn "thirdPatternMatching [1, 2, 3, 4]"
    putStrLn . show $ thirdPatternMatching [1, 2, 3, 4]

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
