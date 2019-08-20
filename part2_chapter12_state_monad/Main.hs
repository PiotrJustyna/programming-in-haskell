-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_state_monad
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Functor

type State = Int

newtype StateTransformer a = S (State -> (a, State))

-- Is this weird/clever or am I just tired?
apply :: StateTransformer a -> State -> (a, State)
apply (S stateTransformer) state = stateTransformer state

-- functor
instance Functor StateTransformer where
    -- fmap :: (a -> b) -> StateTransformer a -> StateTransformer b
    fmap function stateTransformer = S (\state1 -> let (x, state2) = apply stateTransformer state1 in (function x, state2))

main = do
    putStrLn "Ad hoc state transformer (StateTransformer type not used):"
    putStrLn . show . (\x -> let (newValue, newState) = (x + 1, x + 1) in (newValue, newState)) $ (5 :: Int)

    putStrLn ""
    putStrLn "Functor implementation for StateTransformer:"
    putStrLn . show $ apply resultStateTransformer (1 :: Int)
    where
        resultStateTransformer = (+5) <$> S (\x -> (x + 1, x + 1))
