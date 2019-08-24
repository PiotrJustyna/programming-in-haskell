-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_state_monad
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Control.Applicative

type State = Int

newtype StateTransformer a = S (State -> (a, State))

-- Is this weird/clever or am I just tired?
apply :: StateTransformer a -> State -> (a, State)
apply (S transformer) state = transformer state

-- functor
instance Functor StateTransformer where
    -- fmap :: (a -> b) -> StateTransformer a -> StateTransformer b
    fmap function transformer = S (\state1 ->
        let (x, state2) = apply transformer state1 in (function x, state2))

instance Applicative StateTransformer where
    -- pure :: a -> StateTransformer a
    pure x = S (\y -> (x, y))
    -- (<*>) :: StateTransformer (a -> b) -> StateTransformer a -> StateTransformer b
    functionStateTransformer <*> valueStateTransformer = S (\state1 ->
        let (f, state2) = apply functionStateTransformer state1
            (x, state3) = apply valueStateTransformer state2 in (f x, state3))

main = do
    putStrLn "Ad hoc simple value state transformer applied (StateTransformer type not used):"
    putStrLn . show . (\x -> (x, x)) $ (5 :: Int)

    putStrLn ""
    putStrLn "Functor implementation for StateTransformer:"
    putStrLn . show $ (apply ((+5) <$> valueStateTransformer)) (1 :: Int)

    putStrLn ""
    putStrLn "Applicative implementation for StateTransformer, example 1:"
    putStrLn . show $ apply resultStateTransformer1 (1 :: Int)

    -- ^^^ order of operations:
    -- 1. starting state is 1
    -- 2. (state (1) stays unchanged and the state transformer function is returned to be applied to the result of the next step)
    -- 3. (+2) is applied to the state (now the state is 3) and the state transformer's previous version of the state (1) gets incremented by 1. The result is (2, 3)
    -- 4. now the function from step 2 (*2) can finally be applied to the value from step 3 (2), resulting in (4, 3)

    putStrLn ""
    putStrLn "Applicative implementation for StateTransformer, example 2:"
    putStrLn . show $ apply resultStateTransformer2 (1 :: Int)

    -- ^^^ order of operations:
    -- 1. starting state is 1
    -- 2. (state (1) gets multiplied by 3 and the state transformer function is returned to be applied to the result of the next step)
    -- 3. (+2) is applied to the state (now the state is 5) and the state transformer's previous version of the state (3) gets incremented by 1. The result is (4, 5)
    -- 4. now the function from step 2 (*2) can finally be applied to the value from step 3 (4), resulting in (8, 5)


    where
        valueStateTransformer = S (\x -> (x + 1, x + 2))
        functionStateTransformer1 = S (\x -> ((\y -> y * 2), x))
        resultStateTransformer1 = functionStateTransformer1 <*> valueStateTransformer
        functionStateTransformer2 = S (\x -> ((\y -> y * 2), x * 3))
        resultStateTransformer2 = functionStateTransformer2 <*> valueStateTransformer