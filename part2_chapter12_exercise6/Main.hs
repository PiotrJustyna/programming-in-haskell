-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise6
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    -- since in our case (f is 'r ->'):
    -- return :: a -> r -> a
    -- it is immediately obvious that it really is just the const function
    -- Example:
    putStrLn . show $ return 3 4
    putStrLn . show $ const 3 4
    
    -- since in our case (f is 'r ->'):
    -- (>>=)  :: (r -> a) -> (a -> r -> b) -> (r -> b)
    -- Example:
    putStrLn . show $ (>>=) (+1) (\x y -> x) 4      -- 5
    putStrLn . show $ (+1) >>= (\x y -> x + y) $ 4  -- 9