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

    putStrLn "getVariableNames (Const True)"
    putStrLn . show $ getVariableNames (Const True)

    putStrLn "getVariableNames (Var 'A')"
    putStrLn . show $ getVariableNames (Var 'A')

    putStrLn "getVariableNames (Not (Var 'A'))"
    putStrLn . show $ getVariableNames (Not (Var 'A'))

    putStrLn "getVariableNames (And (Var 'A') (Var 'B'))"
    putStrLn . show $ getVariableNames (And (Var 'A') (Var 'B'))

    putStrLn "getVariableNames (Imply (Var 'B') (Var 'A'))"
    putStrLn . show $ getVariableNames (Imply (Var 'B') (Var 'A'))

    putStrLn "generateAllPermutationsOfNBools 1"
    putStrLn . show $ generateAllPermutationsOfNBools 1

    putStrLn "generateAllPermutationsOfNBools 2"
    putStrLn . show $ generateAllPermutationsOfNBools 2

    putStrLn "generateAllPermutationsOfNBools 3"
    putStrLn . show $ generateAllPermutationsOfNBools 3

    putStrLn "generateAllSubstitutions (Var 'A')"
    putStrLn . show $ generateAllSubstitutions (Var 'A')

    putStrLn "generateAllSubstitutions (And (Var 'A') (Var 'B'))"
    putStrLn . show $ generateAllSubstitutions (And (Var 'A') (Var 'B'))

    putStrLn "isTautology (And (Var 'A') (Var 'B'))"
    putStrLn . show $ isTautology (And (Var 'A') (Var 'B'))

    putStrLn "isTautology (Imply (Var 'A') (Var 'A'))"
    putStrLn . show $ isTautology (Imply (Var 'A') (Var 'A'))

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

getVariableNames :: Proposition -> [Char]
getVariableNames (Const _) = []
getVariableNames (Var variableName) = [variableName]
getVariableNames (Not proposition) = getVariableNames proposition
getVariableNames (And proposition1 proposition2) = (getVariableNames proposition1) ++ (getVariableNames proposition2)
getVariableNames (Imply proposition1 proposition2) = (getVariableNames proposition1) ++ (getVariableNames proposition2)

generateAllPermutationsOfNBools :: Int -> [[Bool]]
generateAllPermutationsOfNBools 1 = [[False], [True]]
generateAllPermutationsOfNBools n = [y : x | x <- generateAllPermutationsOfNBools (n - 1), y <- [False, True]]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

generateAllSubstitutions :: Proposition -> [Substitutions]
generateAllSubstitutions proposition = map (zip uniqueVariableNames) (generateAllPermutationsOfNBools (length uniqueVariableNames))
    where
        uniqueVariableNames = removeDuplicates (getVariableNames proposition)

isTautology :: Proposition -> Bool
isTautology proposition = and [evaluate singleCollectionOfSubstitutions proposition | singleCollectionOfSubstitutions <- (generateAllSubstitutions proposition)]