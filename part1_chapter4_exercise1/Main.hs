-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise1
-- :load Main
main = do
    putStrLn "halve [1, 2, 3, 4, 5, 6]"
    putStrLn . show $ halve [1, 2, 3, 4, 5, 6]

    putStrLn "halve [1, 2, 3]"
    putStrLn . show $ halve [1, 2, 3]
    
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
