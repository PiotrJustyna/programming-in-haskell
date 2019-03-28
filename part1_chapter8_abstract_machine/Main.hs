-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_abstract_machine
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "evaluate (Value 5)"
    putStrLn . show $ evaluate (Value 5)

    putStrLn "evaluate (Add (Value 5) (Value 3))"
    putStrLn . show $ evaluate (Add (Value 5) (Value 3))

data Expression = Value Int | Add Expression Expression

evaluate :: Expression -> Int
evaluate (Value n) = n
evaluate (Add expression1 expression2) = (evaluate expression1) + (evaluate expression2)