-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter9_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "allPossibleChoices pool:"
    putStrLn . show $ allPossibleChoices pool

    putStrLn "newAllPossibleChoices pool:"
    putStrLn . show $ newAllPossibleChoices pool

pool :: [Int]
pool = [1, 2, 3]

subSequences :: [a] -> [[a]]
subSequences [] = [[]]
subSequences (x:xs) = ys ++ [x:n | n <- ys]
    where
        ys = subSequences xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x list = [(left y) ++ [x] ++ (right y) | y <- [0 .. (length list)]]
    where
        left n = take n list
        right n = drop n list

permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat (map (interleave x) (permutations xs))

allPossibleChoices :: [a] -> [[a]]
allPossibleChoices = concat . map permutations . subSequences

newAllPossibleChoices :: Eq a => [a] -> [[a]]
newAllPossibleChoices x = [z | y <- subSequences x, z <- permutations y]