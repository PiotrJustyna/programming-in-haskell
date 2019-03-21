-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise7
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.List

type Bit = Int

main = do
    putStrLn "bit2Int [1, 0, 0]"
    putStrLn . show $ bit2Int [1, 0, 0]

    putStrLn "int2bit 4"
    putStrLn . show $ int2bit 1
    putStrLn . show $ int2bit 2
    putStrLn . show $ int2bit 3
    putStrLn . show $ int2bit 4
    putStrLn . show $ int2bit 13
    putStrLn . show $ int2bit 16

bit2Int :: [Bit] -> Int
bit2Int bits = foldr (\x y -> x + (2 * y)) 0 (reverse bits)

-- I'm using "++"" instead of ":" for the sake of simplicity and correct bit ordering.
-- Performance is not my main goal in this exercise.
int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x = (int2bit (x `div` 2)) ++ [x `mod` 2]
