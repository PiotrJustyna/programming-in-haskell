-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise8
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "halve ([] :: [Int])"
    putStrLn . show $ halve ([] :: [Int])

    putStrLn "halve [1]"
    putStrLn . show $ halve [1]

    putStrLn "halve [1, 2]"
    putStrLn . show $ halve [1, 2]

    putStrLn "halve [1, 2, 3]"
    putStrLn . show $ halve [1, 2, 3]

    putStrLn "msort ([] :: [Int])"
    putStrLn . show $ msort ([] :: [Int])

    putStrLn "msort [1]"
    putStrLn . show $ msort [1]

    putStrLn "msort [2, 1]"
    putStrLn . show $ msort [2, 1]

    putStrLn "msort [2, 1, 3, 0, 17, 5, 5, 4, 3, 45]"
    putStrLn . show $ msort [2, 1, 3, 0, 17, 5, 5, 4, 3, 45]

msort :: Ord a => [a] -> [a]
msort [] = []
msort (x:[]) = [x]
msort (x1:x2:[])
    | x1 <= x2 = [x1, x2]
    | otherwise = [x2, x1]
msort x = merge (msort (fst half)) (msort (snd half))
    where
        half = halve x

halve :: [a] -> ([a], [a])
halve x = (take newLength x, drop newLength x)
    where
        newLength = ((length x) `div` 2)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y
merge x [] = x
merge (x:xs) (y:ys)
    | x <= y = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)