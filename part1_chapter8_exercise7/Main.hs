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

    putStrLn "##########"

    putStrLn "demoList1:"
    putStrLn . show $ demoList1

    putStrLn "demoList2:"
    putStrLn . show $ demoList2

    putStrLn "demoList3:"
    putStrLn . show $ demoList3

    putStrLn "demoList2 == demoList2:"
    putStrLn . show $ demoList2 == demoList2

    putStrLn "demoList1 == demoList2:"
    putStrLn . show $ demoList1 == demoList2

    putStrLn "demoList2 == demoList1:"
    putStrLn . show $ demoList2 == demoList1

    putStrLn "demoList1 == demoList3:"
    putStrLn . show $ demoList1 == demoList3

    putStrLn "demoList1 == demoList1:"
    putStrLn . show $ demoList1 == demoList1

demoList1 :: List' Int
demoList1 = Head' 1 (Head' 2 (EmptyList'))

demoList2 :: List' Int
demoList2 = EmptyList'

demoList3 :: List' Int
demoList3 = Head' 1 (Head' 3 (EmptyList'))

data Maybe' a = Just' a | Nothing' deriving Show

data List' a = EmptyList' | Head' a (List' a) deriving Show

instance Eq a => Eq (Maybe' a) where
    (==) Nothing' Nothing' = True
    (==) Nothing' (Just' _) = False
    (==) (Just' _) Nothing' = False
    (==) (Just' x) (Just' y) = x == y

instance Eq a => Eq (List' a) where
     (==) EmptyList' EmptyList' = True
     (==) EmptyList' (Head' _ _) = False
     (==) (Head' _ _) EmptyList' = False
     (==) (Head' i j) (Head' k l) = (i == k) && (j == l)