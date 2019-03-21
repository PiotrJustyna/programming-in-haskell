-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise7
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Data.Char

type Bit = Int

main = do
    putStrLn "bit2Int [1, 0, 0]"
    putStrLn . show $ bit2Int [1, 0, 0]

    putStrLn "int2bit 4"
    putStrLn . show $ int2bit 4

    putStrLn "int2bit 16"
    putStrLn . show $ int2bit 16

    putStrLn "make8 [1, 0, 1]"
    putStrLn . show $ make8 [1, 0, 1]

    putStrLn "encode \"abc\""
    putStrLn . show $ encode "abc"

    putStrLn "chop8 [1, 0, 1, 0, 1, 0, 1, 0, 1, 1]"
    putStrLn . show $ chop8 [1, 0, 1, 0, 1, 0, 1, 0, 1, 1]

    putStrLn "decode [0,1,1,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1]"
    putStrLn . show $ decode [0,1,1,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1]

bit2Int :: [Bit] -> Int
bit2Int bits = foldr (\x y -> x + (2 * y)) 0 (reverse bits)

-- I'm using "++"" instead of ":" for the sake of simplicity and correct bit ordering.
-- Performance is not my main goal in this exercise.
int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x = (int2bit (x `div` 2)) ++ [x `mod` 2]

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (padding ++ bits)
    where
        padding = take (8 - (length bits)) (repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = (take 8 xs) : (chop8 (drop 8 xs))

decode :: [Bit] -> String
decode bits = map (chr . bit2Int . make8) (chop8 bits)