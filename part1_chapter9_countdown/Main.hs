-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter9_countdown
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "Operators:"
    putStrLn . show $ Add
    putStrLn . show $ Subtract
    putStrLn . show $ Multiply
    putStrLn . show $ Divide

    putStrLn "isValid Add 1 3:"
    putStrLn . show $ isValid Add 1 3

    putStrLn "isValid Subtract 1 3:"
    putStrLn . show $ isValid Subtract 1 3

    putStrLn "isValid Subtract 3 1:"
    putStrLn . show $ isValid Subtract 3 1

    putStrLn "isValid Multiply 1 3:"
    putStrLn . show $ isValid Multiply 1 3

    putStrLn "isValid Divide 2 4:"
    putStrLn . show $ isValid Divide 2 4

    putStrLn "isValid Divide 4 2:"
    putStrLn . show $ isValid Divide 4 2

    putStrLn "apply Add 4 2:"
    putStrLn . show $ apply Add 4 2

    putStrLn "apply Subtract 4 2:"
    putStrLn . show $ apply Subtract 4 2

    putStrLn "apply Multiply 4 2:"
    putStrLn . show $ apply Multiply 4 2

    putStrLn "apply Divide 4 2:"
    putStrLn . show $ apply Divide 4 2

    putStrLn "Expression: Value (5 :: Int):"
    putStrLn . show $ Value (5 :: Int)

    putStrLn "Expression: expression1:"
    putStrLn $ show expression1

    putStrLn "values expression1:"
    putStrLn . show $ values expression1

    putStrLn "evaluate expression1:"
    putStrLn . show $ evaluate expression1

    putStrLn "subSequences [1, 2, 3]"
    putStrLn . show $ subSequences [1, 2, 3]

    putStrLn "interleave 4 [1, 2, 3]"
    putStrLn . show $ interleave 4 [1, 2, 3]

    putStrLn "permutations [1, 2, 3]"
    putStrLn . show $ permutations [1, 2, 3]

    putStrLn "choices [1, 2]"
    putStrLn . show $ choices [1, 2]

    putStrLn "choices [1, 2, 3]"
    putStrLn . show $ choices [1, 2, 3]

expression1 :: Expression
expression1 = Application Add (Value (5 :: Int)) (Application Multiply (Value (2 :: Int)) (Value (3 :: Int)))

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

isValid :: Operator -> Int -> Int -> Bool
isValid Add _ _ = True
isValid Subtract x y = x > y
isValid Multiply _ _ = True
isValid Divide x y = x `mod` y == 0

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

choices :: [a] -> [[a]]
choices = concat . map permutations . subSequences