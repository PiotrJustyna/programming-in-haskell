-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "all' (<3) [1, 2]"
    putStrLn . show $ all' (<3) [1, 2]

    putStrLn "all' (<3) [1, 2, 3]"
    putStrLn . show $ all' (<3) [1, 2, 3]

    putStrLn "all'' (<3) [1, 2]"
    putStrLn . show $ all'' (<3) [1, 2]

    putStrLn "all'' (<3) [1, 2, 3]"
    putStrLn . show $ all'' (<3) [1, 2, 3]

    putStrLn "any' (<3) [1, 2]"
    putStrLn . show $ any' (<3) [1, 2]

    putStrLn "any' (<3) [3, 4]"
    putStrLn . show $ any' (<3) [3, 4]

    putStrLn "takeWhile' (<3) [1, 2, 3]"
    putStrLn . show $ takeWhile' (<3) [1, 2]

    putStrLn "takeWhile' (<3) [3, 4, 5]"
    putStrLn . show $ takeWhile' (<3) [3, 4, 5]

    putStrLn "dropWhile' (<3) [1, 2, 3]"
    putStrLn . show $ dropWhile' (<3) [1, 2, 3]

    putStrLn "dropWhile' (<3) [3, 4, 5]"
    putStrLn . show $ dropWhile' (<3) [3, 4, 5]


all' :: (a -> Bool) -> [a] -> Bool
all' f x = foldr (\y z -> (f y) && z) True x

-- while all' works, this is acutally much simpler
all'' :: (a -> Bool) -> [a] -> Bool
all'' predicate = and . (map predicate)

any' :: (a -> Bool) -> [a] -> Bool
any' predicate = or . (map predicate)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' = filter

-- I feel so clever right now.
-- The point free technique borrowed from the composition operator:
-- (.) :: (b->c) -> (a->b) -> (a->c)
-- f . g = \ x -> f (g x)
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' = filter . (\predicate -> not . predicate)
