-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

main = do
    putStrLn "Number of demoTree1's leaves:"
    putStrLn . show $ numberOfLeaves demoTree1

    putStrLn "Is demoTree1 balanced?"
    putStrLn . show $ balanced demoTree1

    putStrLn "Number of demoTree2's leaves:"
    putStrLn . show $ numberOfLeaves demoTree2

    putStrLn "Is demoTree2 balanced?"
    putStrLn . show $ balanced demoTree2

demoTree1 :: Tree Char
demoTree1 = Node (Leaf 'a') (Leaf 'b')

demoTree2 :: Tree Char
demoTree2 = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node x y) = (numberOfLeaves x) == (numberOfLeaves y)

numberOfLeaves :: Tree a -> Int
numberOfLeaves (Leaf _) = 1
numberOfLeaves (Node x y) = (numberOfLeaves x) + (numberOfLeaves y)