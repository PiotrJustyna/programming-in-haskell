-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_state_monad_practice_relabelling_trees
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "New tree:"
    putStrLn . show $ newTree

    putStrLn ""
    putStrLn "Relabelled new tree:"
    putStrLn . show $ relabel newTree 0

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

newTree :: Tree Char
newTree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- I feel this function is not perfectly described in the book,
-- so will try to do a better job here.
-- The function:
-- * assigns an integer to every leaf
-- * once an integer is used, it gets incremented and passed along to the next Tree item
--   that way we have each leaf labelled with a unique integer
relabel :: Tree a -> Int -> (Tree Int, Int)
relabel (Leaf _) n = (Leaf n, n + 1)
relabel (Node left right) n =
    (Node relabelledLeftTree relabelledRightTree, nRight)
    where
        (relabelledLeftTree, nLeft) = relabel left n
        (relabelledRightTree, nRight) = relabel right nLeft