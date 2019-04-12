-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise6
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

    putStrLn "eval exprression1:"
    putStrLn . show $ eval exprression1

    putStrLn "size exprression1:"
    putStrLn . show $ size exprression1

    putStrLn "exprression2:"
    putStrLn . show $ exprression2

    putStrLn "eval exprression2:"
    putStrLn . show $ eval exprression2

    putStrLn "size exprression2:"
    putStrLn . show $ size exprression2

    putStrLn "exprression3:"
    putStrLn . show $ exprression3

    putStrLn "eval exprression3:"
    putStrLn . show $ eval exprression3

    putStrLn "size exprression3:"
    putStrLn . show $ size exprression3

exprression1 :: Expression
exprression1 = Value 6

exprression2 :: Expression
exprression2 = Add (Value 6) (Value 3)

exprression3 :: Expression
exprression3 = Add (Value 6) (Add (Value 1) (Value 3))

folde :: (Int -> a) -> (a -> a -> a) -> Expression -> a
folde f g (Value x) = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

eval :: Expression -> Int
eval = folde id (+)

size :: Expression -> Int
size = folde (\y -> 1) (+)