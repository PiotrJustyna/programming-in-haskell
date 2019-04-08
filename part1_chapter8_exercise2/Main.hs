-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

main = do
    putStrLn "Node demoTree"
    putStrLn . show $ demoTree

    putStrLn "occurs 'c' demoTree"
    putStrLn . show $ occurs 'c' demoTree

    putStrLn "occurs 'd' demoTree"
    putStrLn . show $ occurs 'd' demoTree

    putStrLn "occurs\' 'c' demoTree"
    putStrLn . show $ occurs' 'c' demoTree

    putStrLn "occurs\' 'd' demoTree"
    putStrLn . show $ occurs' 'd' demoTree

demoTree :: Tree Char
demoTree = Node (Leaf 'a') 'b' (Leaf 'c')

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node i j k) = (x == j) || (occurs x i) || (occurs x k)

occurs' :: (Ord a, Eq a) => a -> Tree a -> Bool
occurs' x (Leaf y) = x == y
occurs' x (Node i j k) =
    if (x == j)
        then True
        else if (x < j)
            then (occurs' x i)
            else (occurs' x k)