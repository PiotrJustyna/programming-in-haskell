-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_tautology_checker
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn . show $ Const True
    putStrLn . show $ Var 'A'
    putStrLn . show $ Not (Const True)
    putStrLn . show $ And (Const True) (Const False)
    putStrLn . show $ Imply (Const True) (Const False)

data Proposition =
    Const Bool
    | Var Char
    | Not Proposition
    | And Proposition Proposition
    | Imply Proposition Proposition
    deriving Show