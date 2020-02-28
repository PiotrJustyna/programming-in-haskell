-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main

-- This exercise doesn't need a solution as the code would not compile.
-- It would not compile, because it clashes with:

-- instance Applicative ((->) r) where
--     pure = const
--     (<*>) f g x = f x (g x)
--     liftA2 q f g x = q (f x) (g x)

-- For reference:

-- class (Functor f) => Applicative f where
--     pure  :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- TODO
main = do
    -- since in our case (f is 'r ->'):
    -- pure :: a -> r -> a
    -- it is immediately obvious that it really is just the const function
    -- Example:
    putStrLn . show $ pure 3 4
    putStrLn . show $ const 3 4
    
    -- since in our case (f is 'r ->'):
    -- (<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
    -- Example:
    putStrLn . show $ (<*>) (\x y -> x) (+1) 4      -- 4
    putStrLn . show $ (\x y -> x + y) <*> (+1) $ 4  -- 9