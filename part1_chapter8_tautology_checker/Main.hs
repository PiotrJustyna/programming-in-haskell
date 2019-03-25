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

    putStrLn "evaluate substitutions (Const True)"
    putStrLn . show $ evaluate substitutions (Const True)

    putStrLn "evaluate ([] :: Substitution) (Const True)"
    putStrLn . show $ evaluate ([] :: Substitution) (Const True)

    where
        substitutions = [('A', True), ('B', False)]

type Association key value = [(key, value)]

type Substitution = Association Char Bool

data Proposition =
    Const Bool
    | Var Char
    | Not Proposition
    | And Proposition Proposition
    | Imply Proposition Proposition
    deriving Show

evaluate :: Substitution -> Proposition -> Bool
evaluate _ (Const x) = x