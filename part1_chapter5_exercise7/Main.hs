-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise7
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn $ show [(x, y) | x <- [1, 2], y <- [3, 4]]
    putStrLn . show $ concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]