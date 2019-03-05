-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn . show $ grid 1 2

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | y <- [0 .. n], x <- [0 .. m]]