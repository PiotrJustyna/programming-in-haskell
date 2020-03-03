-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise3
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

variable = Var 5
value = Val 5
addition = Add variable value

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x) = Var (f x)
    fmap f (Val x) = Val x
    fmap f (Add x1 x2) = Add (fmap f x1) (fmap f x2)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure = Var

    -- <*> :: Expr (a -> b) -> Expr a -> Expr b
    (<*>) (Var f) (Var x) = Var (f x)
    (<*>) (Var f) (Val x) = Val x
    (<*>) (Var f) (Add x1 x2) = Add (fmap f x1) (fmap f x2)
    -- technically, one could also squeeze in (Var f) into Add's expressions,
    -- but I think that would be an overkill

instance Monad Expr where
    -- return :: a -> Expr a
    return = Var

    -- <*> :: Expr a -> (a -> Expr b) -> Expr b
    (>>=) (Var x) f = f x
    (>>=) (Val x) f = Val x
    (>>=) (Add x1 x2) f = Add (x1 >>= f) (x2 >>= f)

main = do
    putStrLn "Values:"
    putStrLn . show $ "variable: " ++ (show variable)
    putStrLn . show $ "value: " ++ (show (value :: Expr Int))
    putStrLn . show $ "addition: " ++ (show (addition :: Expr Int))

    putStrLn "---"

    putStrLn "Functor examples:"
    putStrLn . show $ "fmap (+1) variable: " ++ (show $ fmap (+1) variable)
    putStrLn . show $ "fmap (+1) value: " ++ (show $ fmap (+1) value)
    putStrLn . show $ "fmap (+1) addition: " ++ (show $ fmap (+1) addition)

    putStrLn "---"

    putStrLn "Applicative examples:"
    putStrLn . show $ "(Var (+1)) <*> variable: " ++ (show $ (Var (+1)) <*> variable)
    putStrLn . show $ "(Var (+1)) value: " ++ (show $ (Var (+1)) <*> value)
    putStrLn . show $ "(Var (+1)) addition: " ++ (show $ (Var (+1)) <*> addition)

    putStrLn "---"

    putStrLn "Monad examples:"
    putStrLn . show $ "variable >>= (\\x -> Var (x + 1)): " ++ (show $ variable >>= (\x -> Var (x + 1)))
    putStrLn . show $ "value >>= (\\x -> Var (x + 1)): " ++ (show $ value >>= (\x -> Var (x + 1)))
    putStrLn . show $ "addition >>= (\\x -> Var (x + 1)): " ++ (show $ addition >>= (\x -> Var (x + 1)))