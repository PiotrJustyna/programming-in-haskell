-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise9
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char

type Bit = Int

main = do
    putStrLn "altMap (+1) (+2) ([] :: [Int])"
    putStrLn . show $ altMap (+1) (+2) ([] :: [Int])

    putStrLn "altMap (+1) (+2) [1]"
    putStrLn . show $ altMap (+1) (+2) [1]

    putStrLn "altMap (+1) (+2) [1, 2]"
    putStrLn . show $ altMap (+1) (+2) [1, 2]

    putStrLn "altMap (+1) (+2) [1, 2, 3]"
    putStrLn . show $ altMap (+1) (+2) [1, 2, 3]

    putStrLn "altMap (+1) (+2) [1, 2, 3, 4]"
    putStrLn . show $ altMap (+1) (+2) [1, 2, 3, 4]

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f _ (x:[]) = [f x]
altMap f g (x1:x2:xs) = (f x1) : (g x2) : (altMap f g xs)