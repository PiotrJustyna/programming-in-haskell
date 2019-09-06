-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_state_monad_practice_relabelling_trees
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
type State = Int

newtype StateTransformer a = S (State -> (a, State))

-- Is this weird/clever or am I just tired?
apply :: StateTransformer a -> State -> (a, State)
apply (S transformer) state = transformer state

-- functor
instance Functor StateTransformer where
    -- fmap :: (a -> b) -> StateTransformer a -> StateTransformer b
    fmap function transformer = S (\state1 ->
        let (x, state2) = apply transformer state1 in (function x, state2))

-- applicative
instance Applicative StateTransformer where
    -- pure :: a -> StateTransformer a
    pure x = S (\y -> (x, y))
    -- (<*>) :: StateTransformer (a -> b) -> StateTransformer a -> StateTransformer b
    functionStateTransformer <*> valueStateTransformer = S (\state1 ->
        let (f, state2) = apply functionStateTransformer state1
            (x, state3) = apply valueStateTransformer state2 in (f x, state3))

main = do
    putStrLn "New tree:"
    putStrLn . show $ newTree

    putStrLn ""
    putStrLn "Relabelled new tree:"
    putStrLn . show $ relabel newTree 0

    putStrLn ""
    putStrLn "Relabelled new tree, applicative style:"
    putStrLn . show $ apply (applicativeRelabel newTree) 0

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

-- applicative relabel version
fresh :: StateTransformer Int
fresh = S (\x -> (x, x + 1))

applicativeRelabel :: Tree a -> StateTransformer (Tree Int)
applicativeRelabel (Leaf _) = Leaf <$> fresh
applicativeRelabel (Node left right) = Node <$> applicativeRelabel left <*> applicativeRelabel right