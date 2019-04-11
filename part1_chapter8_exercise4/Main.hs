-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise4
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

main = do
    putStrLn "list1:"
    putStrLn . show $ list1

    putStrLn "split list1:"
    putStrLn . show $ split list1

    -- commented out as it is supposed to cause an error
    -- putStrLn "balance list1:"
    -- putStrLn . show $ balance list1

    putStrLn "list2:"
    putStrLn . show $ list2

    putStrLn "split list2:"
    putStrLn . show $ split list2

    putStrLn "balance list2:"
    putStrLn . show $ balance list2

    putStrLn "list3:"
    putStrLn . show $ list3
    
    putStrLn "split list3:"
    putStrLn . show $ split list3

    putStrLn "balance list3:"
    putStrLn . show $ balance list3

    putStrLn "list4:"
    putStrLn . show $ list4

    putStrLn "split list4:"
    putStrLn . show $ split list4

    putStrLn "balance list4:"
    putStrLn . show $ balance list4

list1 :: [Int]
list1 = []

list2 :: [Int]
list2 = [1]

list3 :: [Int]
list3 = [1, 2, 3, 4]

list4 :: [Int]
list4 = [1, 2, 3, 4, 5]

split :: Show a => [a] -> ([a], [a])
split [] = ([], [])
split (x:[]) = ([x], [])
split xs = (take half xs, drop half xs)
    where
        -- I'm adding mod just to allocate
        -- the odd item in the first part of the tuple
        half = ((length xs) `div` 2) + ((length xs) `mod` 2)

balance :: Show a => [a] -> Tree a
balance [] = error "Cannot form a tree from an empty list."
balance (x:[]) = Leaf x
balance xs = Node (balance left) (balance right)
    where
        (left, right) = split xs