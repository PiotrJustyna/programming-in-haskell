-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise9
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main

main = do
    putStrLn "evaluateExpression (Value 5)"
    putStrLn . show $ evaluateExpression (Value 5)

    putStrLn "evaluateExpression (Add (Value 5) (Value 3))"
    putStrLn . show $ evaluateExpression (Add (Value 5) (Value 3))

    putStrLn "execute 3 [Addition 6]"
    putStrLn . show $ execute 3 [Addition 6]

    putStrLn "execute 3 [Addition 6, Evaluation (Value 5)]"
    putStrLn . show $ execute 3 [Addition 6, Evaluation (Value 5)]

    putStrLn "evaluateExpression (Multiply (Value 5) (Value 3))"
    putStrLn . show $ evaluateExpression (Multiply (Value 5) (Value 3))

    putStrLn "execute 3 [Multiplication 6]"
    putStrLn . show $ execute 3 [Multiplication 6]

    putStrLn "execute 3 [Multiplication 6, Evaluation (Value 5)]"
    putStrLn . show $ execute 3 [Multiplication 6, Evaluation (Value 5)]

    putStrLn "execute 3 [Multiplication 6, Addition 1, Evaluation (Value 5)]"
    putStrLn . show $ execute 3 [Multiplication 6, Addition 1, Evaluation (Value 5)]

data Expression = Value Int | Add Expression Expression | Multiply Expression Expression

data Operation = Evaluation Expression | Addition Int | Multiplication Int

type ControlStack = [Operation]

evaluateExpression :: Expression -> Int
evaluateExpression (Value x) = x
evaluateExpression (Add expression1 expression2) = (evaluateExpression expression1) + (evaluateExpression expression2)
evaluateExpression (Multiply expression1 expression2) = (evaluateExpression expression1) * (evaluateExpression expression2)

evaluateExpressionWithControlStack :: Expression -> ControlStack -> Int
evaluateExpressionWithControlStack (Value x) y = execute x y
evaluateExpressionWithControlStack (Add expression1 expression2) y = evaluateExpressionWithControlStack expression1 ((Evaluation expression2) : y)
evaluateExpressionWithControlStack (Multiply expression1 expression2) y = evaluateExpressionWithControlStack expression1 ((Evaluation expression2) : y)

execute :: Int -> ControlStack -> Int
execute x [] = x
execute x ((Evaluation expression) : controlStack) = evaluateExpressionWithControlStack expression ((Addition x) : controlStack)
execute x ((Addition y) : controlStack) = execute (x + y) controlStack
execute x ((Multiplication y) : controlStack) = execute (x * y) controlStack