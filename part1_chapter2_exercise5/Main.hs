-- ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter2_exercise5
-- :load Main
main = do
    putStrLn $ "init: " ++ (show $ init [1, 2, 3, 4, 5])
    putStrLn $ "alternative init 1: " ++ (show $ alternativeInit1 [1, 2, 3, 4, 5])
    putStrLn $ "alternative init 2: " ++ (show $ alternativeInit2 [1, 2, 3, 4, 5])
    putStrLn $ "alternative init 3: " ++ (show $ alternativeInit3 [1, 2, 3, 4, 5])

alternativeInit1 :: [a] -> [a]
alternativeInit1 x = reverse $ tail $ reverse x

alternativeInit2 :: [a] -> [a]
alternativeInit2 []     = error "empty list"
alternativeInit2 (x:[]) = []
alternativeInit2 (x:xs) = x:(alternativeInit2 xs)

alternativeInit3 :: [a] -> [a]
alternativeInit3 x = reverse $ drop 1 $ reverse x