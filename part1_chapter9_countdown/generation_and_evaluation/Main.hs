-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter9_countdown\generation_and_evaluation
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "allPossibleSolutions [1, 3, 7, 10, 25, 50] 765"
    putStrLn . show $ allPossibleSolutions [1, 3, 7, 10, 25, 50] 765

    putStrLn "allPossibleSolutions [1, 2, 3] 6"
    putStrLn . show $ allPossibleSolutions [1, 2, 3] 6

expression1 :: Expression
expression1 = Application Add (Value (5 :: Int)) (Application Multiply (Value (2 :: Int)) (Value (3 :: Int)))

expression2 :: Expression
expression2 = Application
    Multiply
    (Application Add (Value (1 :: Int)) (Value (50 :: Int)))
    (Application Subtract (Value (25 :: Int)) (Value (10 :: Int)))

data Operator = Add | Subtract | Multiply | Divide

instance Show Operator where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

data Expression = Value Int | Application Operator Expression Expression

instance Show Expression where
    show (Value x) = show x
    show (Application operator expression1 expression2) =
        (brackets expression1) ++ show operator ++ (brackets expression2)
        where
            brackets (Value x) = show x
            brackets application = "(" ++ show application ++ ")"

type Result = (Expression, Int)

isValidOperation :: Operator -> Int -> Int -> Bool
isValidOperation Add _ _ = True
isValidOperation Subtract x y = x > y
isValidOperation Multiply _ _ = True
isValidOperation Divide x y = y /= 0 && x `mod` y == 0

apply :: Operator -> Int -> Int -> Int
apply Add x y = x + y
apply Subtract x y = x - y
apply Multiply x y = x * y
apply Divide x y = x `div` y

values :: Expression -> [Int]
values (Value x) = [x]
values (Application operator expression1 (expression2)) = (values expression1) ++ (values expression2)

evaluate :: Expression -> Int
evaluate (Value x) =
    if (x > 0)
        then x
        else error "Expression value should be positive."
evaluate (Application operator expression1 expression2) =
    apply operator (evaluate expression1) (evaluate expression2)

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

allPossibleSplits :: [a] -> [([a], [a])]
allPossibleSplits list = [splitAt pivot list | pivot <- [1 .. ((length list) - 1)]]

allPossibleResults :: [Int] -> [Result]
allPossibleResults [] = []
allPossibleResults [x] = [((Value x), x)]
allPossibleResults numbers =
    [bothSidesResults |
        (leftSideValues, rightSideValues) <- allPossibleSplits numbers,
        leftSideResult <- allPossibleResults leftSideValues,
        rightSideResult <- allPossibleResults rightSideValues,
        bothSidesResults <- combine leftSideResult rightSideResult]

combine :: Result -> Result -> [Result]
combine (expression1, value1) (expression2, value2) =
    [(Application operator expression1 expression2, apply operator value1 value2) |
        operator <- [Add, Subtract, Multiply, Divide],
        isValidOperation operator value1 value2]

allPossibleSolutions :: [Int] -> Int -> [Expression]
allPossibleSolutions numbers target =
    [expression |
        singleChoice <- allPossibleChoices numbers,
        (expression, value) <- allPossibleResults singleChoice,
        value == target]
