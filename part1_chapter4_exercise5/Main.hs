-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter4_exercise5
-- option 2 (stack):
-- stack ghci
--
-- :load Main
main = do
    putStrLn "weirdAnd True True"
    putStrLn . show $ weirdAnd True True

    putStrLn "weirdAnd True False"
    putStrLn . show $ weirdAnd True False

    putStrLn "weirdAnd False True"
    putStrLn . show $ weirdAnd False True

    putStrLn "weirdAnd False False"
    putStrLn . show $ weirdAnd False False

-- Without using any other library functions or operators,
-- show how the meaning of the following pattern matching
-- definition for logical conjunction && can be formalised
-- using conditional expressions:
-- True && True = True
-- _ && _ = False
-- Hint: use two nested conditional expressions.
weirdAnd :: Bool -> Bool -> Bool
weirdAnd x y =
    if x
    then
        if y
        then True
        else False
    else False
