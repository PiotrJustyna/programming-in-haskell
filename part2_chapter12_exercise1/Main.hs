-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- from: https://wiki.haskell.org/Functor
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--     (<$) :: a -> f b -> f a

instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node left center right) = Node (fmap f left) (f center) (fmap f right)

    (<$) x Leaf = Leaf
    (<$) x (Node left center right) = Node (x <$ left) x (x <$ right)

sampleLeftBranch = Node Leaf 1 Leaf

sampleRightBranch = Node Leaf 3 Leaf

sampleTree = Node sampleLeftBranch 2 sampleRightBranch

main = do
    putStrLn . show $ fmap (+1) Leaf
    putStrLn . show $ fmap (+1) sampleTree

    putStrLn . show $ 1 <$ Leaf
    putStrLn . show $ 1 <$ sampleTree
