-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise5
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
data Expression = Value Int | Add Expression Expression deriving Show

main = do
    putStrLn "exprression1:"
    putStrLn . show $ exprression1

    putStrLn "folde (+1) (\\x y -> x + y) exprression1:"
    putStrLn . show $ folde (+1) (\x y -> x * y) exprression1

    putStrLn "exprression2:"
    putStrLn . show $ exprression2

    putStrLn "folde (+1) (\\x y -> x * y) exprression2:"
    putStrLn . show $ folde (+1) (\x y -> x * y) exprression2

    putStrLn "exprression3:"
    putStrLn . show $ exprression3

    putStrLn "folde (+1) (\\x y -> x * y) exprression3:"
    putStrLn . show $ folde (+1) (\x y -> x * y) exprression3

exprression1 :: Expression
exprression1 = Value 6

exprression2 :: Expression
exprression2 = Add (Value 6) (Value 3)

exprression3 :: Expression
exprression3 = Add (Value 6) (Add (Value 1) (Value 3))

folde :: (Int -> a) -> (a -> a -> a) -> Expression -> a
folde f g (Value x) = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)