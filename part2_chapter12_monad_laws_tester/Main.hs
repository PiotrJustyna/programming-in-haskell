-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_monad_laws_tester
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "Monad law 1:"
    putStrLn "return x >>= f = f x"
    putStrLn "---"

    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ (Just 5) >>= f
    putStrLn . show $ f 5

    putStrLn ""
    putStrLn "Example 2 (do notation):" -- I am not super convinced it illustrates the problem best, though
    putStrLn "---"
    x' <- return 5
    putStrLn . show $ (+1) x'
    putStrLn . show $ (+1) 5

    putStrLn "Monad law 2:"
    putStrLn "mx >>= return = mx"
    putStrLn "---"

    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ (Just 5) >>= return
    putStrLn . show $ (Just 5)

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn . show $ (Nothing :: Maybe Int) >>= return
    putStrLn . show $ (Nothing :: Maybe Int)

    putStrLn "Monad law 3:"
    putStrLn "(mx >>= f) >>= g = mx >>= (\\x -> (f x >>= g))"
    putStrLn "---"

    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ ((Just 5) >>= f) >>= g
    putStrLn . show $ (Just 5) >>= (\x -> (f x >>= g))

    where
        f = (\x -> Just (x + 1))
        g = (\x -> Just (x * 2))