-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise8
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Control.Applicative

type State = Int

newtype StateTransformer a = S (State -> (a, State))

apply :: StateTransformer a -> State -> (a, State)
apply (S transformer) state = transformer state

-- functor
instance Functor StateTransformer where
    -- fmap :: (a -> b) -> StateTransformer a -> StateTransformer b
    fmap function transformer = transformer >>= (\x -> return (function x))

-- applicative
instance Applicative StateTransformer where
    -- pure :: a -> StateTransformer a
    pure x = S (\y -> (x, y))
    -- (<*>) :: StateTransformer (a -> b) -> StateTransformer a -> StateTransformer b
    functionStateTransformer <*> valueStateTransformer = do
        function <- functionStateTransformer
        valueStateTransformer >>= (\x -> return (function x))

-- monad
instance Monad StateTransformer where
    -- return :: a -> StateTransformer a
    return = pure
    -- (>>=) :: (StateTransformer a) -> (a -> StateTransformer b) -> (StateTransformer b)
    valueStateTransformer >>= function = S (\state1 ->
        let (value, state2) = apply valueStateTransformer state1 in apply (function value) state2)

main = do
    putStrLn "Functor implementation for StateTransformer:"
    putStrLn . show $ (apply ((+5) <$> valueStateTransformer)) (1 :: Int)

    putStrLn "Applicative implementation for StateTransformer, example 1:"
    putStrLn . show $ apply resultStateTransformer1 (1 :: Int)

    putStrLn "Applicative implementation for StateTransformer, example 2:"
    putStrLn . show $ apply resultStateTransformer2 (1 :: Int)

    where
        valueStateTransformer = S (\x -> (x + 1, x + 2))
        functionStateTransformer1 = S (\x -> ((\y -> y * 2), x))
        resultStateTransformer1 = functionStateTransformer1 <*> valueStateTransformer
        functionStateTransformer2 = S (\x -> ((\y -> y * 2), x * 3))
        resultStateTransformer2 = functionStateTransformer2 <*> valueStateTransformer