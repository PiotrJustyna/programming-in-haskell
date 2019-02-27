-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter2_exercise4
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn $ "last: " ++ (show $ last [1, 2, 3, 4, 5])
    putStrLn $ "alternative last: " ++ (show $ alternativeLast [1, 2, 3, 4, 5])

alternativeLast :: [a] -> a
alternativeLast x = head $ reverse x