-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main

-- This exercise doesn't need a solution as the code would not compile.
-- It would not compile, because it clashes with:

-- instance Functor ((->) r) where
--     fmap = (.)

-- It just needs some samples.
-- For reference, this is the dot (composition) operator:

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

-- My brain just got permanently damaged.
-- I do not fully understand this, but it intuitively makes sense:
-- a partially applied function type (a ->) is a functor when composition is its fmap.
-- Then, to demonstrate that it works, we apply two arguments to fmap:
-- * a function
-- * a function

-- from: https://wiki.haskell.org/Functor
-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
--     (<$) :: a -> f b -> f a

main = do
    -- if I understand the exercise correctly:
    putStrLn . show $ (fmap (+1) (*2)) 5
    putStrLn . show $ ((+1) . (*2)) 5
    putStrLn . show $ ((+1) <$> (*2)) 5
    putStrLn . show $ (fmap (show) (+1)) 3