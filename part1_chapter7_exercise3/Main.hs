-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "map' (+1) [1, 2, 3]"
    putStrLn . show $ map' (+1) [1, 2, 3]

    putStrLn "filter' (<3) [1, 2, 3]"
    putStrLn . show $ filter' (<3) [1, 2, 3]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x y -> if (f x) then (x : y) else y) []