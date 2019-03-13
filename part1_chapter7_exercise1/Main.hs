-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "comprehension (+1) (<3) [1, 2, 3, 4, 5]"
    putStrLn . show $ comprehension (+1) (<3) [1, 2, 3, 4, 5]

    putStrLn "mapFilter (+1) (<3) [1, 2, 3, 4, 5]"
    putStrLn . show $ mapFilter (+1) (<3) [1, 2, 3, 4, 5]

comprehension :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
comprehension f p xs = [f x | x <- xs, p x]

mapFilter :: Num a => (a -> a) -> (a -> Bool) -> [a] -> [a]
mapFilter f p xs = map f (filter p xs) 