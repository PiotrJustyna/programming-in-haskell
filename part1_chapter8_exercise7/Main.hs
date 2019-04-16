-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter8_exercise7
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main

main = do
    putStrLn "(Just' 1) == (Just' 2)"
    putStrLn . show $ (Just' (1 :: Int)) == (Just' (2 :: Int))

    putStrLn "(Just' 2) == (Just' 2)"
    putStrLn . show $ (Just' (2 :: Int)) == (Just' (2 :: Int))

    putStrLn "Nothing' == (Just' 2)"
    putStrLn . show $ Nothing' == (Just' (2 :: Int))

    putStrLn "(Just' 2) == Nothing'"
    putStrLn . show $ (Just' (2 :: Int)) == Nothing'

    putStrLn "Nothing' == Nothing'"
    putStrLn . show $ Nothing' == (Nothing' :: (Maybe' Int))

data Maybe' a = Just' a | Nothing' deriving Show

instance Eq a => Eq (Maybe' a) where
    (==) Nothing' Nothing' = True
    (==) Nothing' (Just' _) = False
    (==) (Just' _) Nothing' = False
    (==) (Just' x) (Just' y) = x == y