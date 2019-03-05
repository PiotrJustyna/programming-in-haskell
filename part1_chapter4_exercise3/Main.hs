-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "safetailConditionalExpression [1, 2, 3]"
    putStrLn . show $ safetailConditionalExpression [1, 2, 3]

    putStrLn "safetailConditionalExpression []"
    putStrLn . show $ safetailConditionalExpression ([] :: [Int])

    putStrLn "safetailGuardedEquations [1, 2, 3]"
    putStrLn . show $ safetailGuardedEquations [1, 2, 3]

    putStrLn "safetailGuardedEquations []"
    putStrLn . show $ safetailGuardedEquations ([] :: [Int])

    putStrLn "safetailPatternMatching [1, 2, 3]"
    putStrLn . show $ safetailPatternMatching [1, 2, 3]

    putStrLn "safetailPatternMatching []"
    putStrLn . show $ safetailPatternMatching ([] :: [Int])

-- Consider a function safetail :: [a] -> [a]
-- that behaves in the same way as tail except
-- that it maps the empty list to itself
-- rather than producing an error.
-- Using tail and the function null :: [a] -> Bool
-- that decides if a list is empty or not,
-- define safetail using:
-- a.​a conditional expression;
-- b.​guarded equations;
-- c.​pattern matching.
safetailConditionalExpression :: [a] -> [a]
safetailConditionalExpression xs =
    if (null xs)
        then xs
        else tail xs

safetailGuardedEquations :: [a] -> [a]
safetailGuardedEquations xs
    | null xs = xs
    | otherwise = tail xs

safetailPatternMatching :: [a] -> [a]
safetailPatternMatching [] = []
safetailPatternMatching (_:xs) = xs