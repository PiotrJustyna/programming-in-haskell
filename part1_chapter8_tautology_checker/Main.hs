-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_tautology_checker
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "Propositions:"
    putStrLn . show $ Const True
    putStrLn . show $ Var 'A'
    putStrLn . show $ Not (Const True)
    putStrLn . show $ And (Const True) (Const False)
    putStrLn . show $ Imply (Const True) (Const False)

    putStrLn "find 'A' substitutions"
    putStrLn . show $ find 'A' substitutions

    putStrLn "evaluate substitutions (Const True)"
    putStrLn . show $ evaluate substitutions (Const True)

    putStrLn "evaluate ([] :: Substitutions) (Const True)"
    putStrLn . show $ evaluate ([] :: Substitutions) (Const True)

    putStrLn "evaluate substitutions (Var 'A')"
    putStrLn . show $ evaluate substitutions (Var 'A')

    putStrLn "evaluate substitutions (Var 'B')"
    putStrLn . show $ evaluate substitutions (Var 'B')

    putStrLn "evaluate substitutions (Not (Var 'A'))"
    putStrLn . show $ evaluate substitutions (Not (Var 'A'))

    putStrLn "evaluate substitutions (Not (Var 'B'))"
    putStrLn . show $ evaluate substitutions (Not (Var 'B'))

    putStrLn "evaluate substitutions (And (Var 'A') (Var 'A'))"
    putStrLn . show $ evaluate substitutions (And (Var 'A') (Var 'A'))

    putStrLn "evaluate substitutions (And (Var 'A') (Var 'B'))"
    putStrLn . show $ evaluate substitutions (And (Var 'A') (Var 'B'))

    putStrLn "evaluate substitutions (Imply (Var 'A') (Var 'A'))"
    putStrLn . show $ evaluate substitutions (Imply (Var 'A') (Var 'A'))

    putStrLn "evaluate substitutions (Imply (Var 'B') (Var 'B'))"
    putStrLn . show $ evaluate substitutions (Imply (Var 'B') (Var 'B'))

    putStrLn "evaluate substitutions (Imply (Var 'A') (Var 'B'))"
    putStrLn . show $ evaluate substitutions (Imply (Var 'A') (Var 'B'))

    putStrLn "evaluate substitutions (Imply (Var 'B') (Var 'A'))"
    putStrLn . show $ evaluate substitutions (Imply (Var 'B') (Var 'A'))

    where
        substitutions = [('A', True), ('B', False)]

type Association key value = (key, value)

type Associations key value = [Association key value]

type Substitutions = Associations Char Bool

data Proposition =
    Const Bool
    | Var Char
    | Not Proposition
    | And Proposition Proposition
    | Imply Proposition Proposition
    deriving Show

find :: Char -> Substitutions -> Bool
find x substitutions = snd $ head (filter (\(key, value) -> x == key) substitutions)

evaluate :: Substitutions -> Proposition -> Bool
evaluate _ (Const constantValue) = constantValue
evaluate substitutions (Var variableName) = find variableName substitutions
evaluate substitutions (Not proposition) = not (evaluate substitutions proposition)
evaluate substitutions (And proposition1 proposition2) = (evaluate substitutions proposition1) && (evaluate substitutions proposition2)
evaluate substitutions (Imply proposition1 proposition2) = (evaluate substitutions proposition1) <= (evaluate substitutions proposition2)