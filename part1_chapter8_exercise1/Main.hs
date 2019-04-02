-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise1
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "add Zero (Successor (Successor Zero))"                                                -- 0 + 2
    putStrLn . show $ add Zero (Successor (Successor Zero))

    putStrLn "multiply Zero Zero"                                                                   -- 0 * 0
    putStrLn . show $ multiply Zero Zero

    putStrLn "multiply Zero (Successor Zero)"                                                       -- 0 * 1
    putStrLn . show $ multiply Zero (Successor Zero)

    putStrLn "multiply (Successor Zero) (Successor (Successor Zero))"                               -- 1 * 2
    putStrLn . show $ multiply (Successor Zero) (Successor (Successor Zero))

    putStrLn "multiply (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))"       -- 2 * 3
    putStrLn . show $ multiply (Successor (Successor Zero)) (Successor (Successor (Successor Zero)))

data Nat = Zero | Successor Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero x = x
add (Successor x) y = Successor (add x y)

multiply :: Nat -> Nat -> Nat
multiply Zero y = Zero
multiply (Successor Zero) y = y
multiply (Successor x) y = multiply x (add y y)