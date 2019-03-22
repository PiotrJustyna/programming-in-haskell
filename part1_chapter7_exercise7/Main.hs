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

    putStrLn "encodewithParityBit \"abc\""
    putStrLn . show $ encodewithParityBit "abc"

    putStrLn "chop8 [1, 0, 1, 0, 1, 0, 1, 0, 1, 1]"
    putStrLn . show $ chop8 [1, 0, 1, 0, 1, 0, 1, 0, 1, 1]

    putStrLn "decode [0,1,1,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1]"
    putStrLn . show $ decode [0,1,1,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1]

    putStrLn "decodeWithParityBit [0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0]"
    putStrLn . show $ decodeWithParityBit [0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,0,1,0,1,1,0,0,0,1,1,0]

    putStrLn "transmit \"Haskell\""
    putStrLn . show $ transmit "Haskell"

    putStrLn "addParityBit [0, 0, 0]"
    putStrLn . show $ addParityBit [0, 0, 0]

    putStrLn "addParityBit [1, 1, 0]"
    putStrLn . show $ addParityBit [1, 1, 0]

    putStrLn "addParityBit [1, 1, 1]"
    putStrLn . show $ addParityBit [1, 1, 1]

    putStrLn "transmitWithParityBit \"Haskell\""
    putStrLn . show $ transmitWithParityBit "Haskell"

bit2Int :: [Bit] -> Int
bit2Int bits = foldr (\x y -> x + (2 * y)) 0 (reverse bits)

-- I'm using "++"" instead of ":" for the sake of simplicity and correct bit ordering.
-- Performance is not my main goal in this exercise.
int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit x = (int2bit (x `div` 2)) ++ [x `mod` 2]

make8 :: [Bit] -> [Bit]
make8 = makeN 8

make9 :: [Bit] -> [Bit]
make9 = makeN 9

makeN :: Int -> [Bit] -> [Bit]
makeN n bits = take n (padding ++ bits)
    where
        padding = take (n - (length bits)) (repeat 0)

addParityBit :: [Bit] -> [Bit]
addParityBit xs = xs ++ [parityBit]
    where
        numberOfOnes = length (filter (==1) xs)
        parityBit = numberOfOnes `mod`2

encode :: String -> [Bit]
encode = concat . map (make8 . int2bit . ord)

encodewithParityBit :: String -> [Bit]
encodewithParityBit = concat . map (addParityBit . make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 = chopN 8

chop9 :: [Bit] -> [[Bit]]
chop9 = chopN 9

chopN :: Int -> [Bit] -> [[Bit]]
chopN _ [] = []
chopN n xs = (take n xs) : (chopN n (drop n xs))

decode :: [Bit] -> String
decode bits = map (chr . bit2Int . make8) (chop8 bits)

decodeWithParityBit :: [Bit] -> String
decodeWithParityBit bits = map (chr . bit2Int . checkParityBit . make9) (chop9 bits)

checkParityBit :: [Bit] -> [Bit]
checkParityBit xs = if (calculatedParityBit == givenParityBit) then dataWithoutTheParityBit else (error "Parity bit does not match.")
        where
            dataWithoutTheParityBit = init xs
            numberOfOnes = length (filter (==1) dataWithoutTheParityBit)
            calculatedParityBit = numberOfOnes `mod`2
            givenParityBit = last xs

transmit :: String -> String
transmit = decode . channel . encode

transmitWithParityBit :: String -> String
transmitWithParityBit = decodeWithParityBit . channel . encodewithParityBit

channel :: a -> a
channel = id