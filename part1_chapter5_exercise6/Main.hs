-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise6
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn "factors 6"
    putStrLn . show $ factors 6

    putStrLn "perfects 500"
    putStrLn . show $ perfects 500

factors :: Int -> [Int]
factors x = [y | y <- [1 .. (x - 1)], x `mod` y == 0]

perfects :: Int -> [Int]
perfects limit = [x | x <- [2 .. limit], sum (factors x) == x]