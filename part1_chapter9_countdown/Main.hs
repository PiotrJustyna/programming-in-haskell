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

data Operator = Add | Subtract | Multiply | Divide

instance Show Operator where
    show Add = "+"
    show Subtract = "-"
    show Multiply = "*"
    show Divide = "/"

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