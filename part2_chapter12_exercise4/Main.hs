-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_exercise4
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z (x:xs)) = Z ((g x) : fmap g xs)
    fmap g (Z []) = Z []

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z (g:gs)) <*> (Z (x:xs)) = do
        Z ((g x) : gxs)
        where
            Z gxs = (Z gs) <*> (Z xs)
    (Z []) <*> (Z _) = Z []
    (Z _) <*> (Z []) = Z []

sampleZipListOfValues = Z [1, 2, 3]
sampleLongZipListOfValues = Z [1, 2, 3, 4, 5]
sampleZipListOfFunctions = Z [(+4), (+5), (+6)]

main = do
    putStrLn . show $ sampleZipListOfValues
    putStrLn . show $ fmap (+1) sampleZipListOfValues
    -- commented out for obvious reasons:
    -- putStrLn . show $ ((pure 5) :: ZipList Int)
    putStrLn . show $ sampleZipListOfFunctions <*> sampleZipListOfValues
    putStrLn . show $ sampleZipListOfFunctions <*> sampleLongZipListOfValues