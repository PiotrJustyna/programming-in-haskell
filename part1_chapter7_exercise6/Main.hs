-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise6
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
type Bit = Int

main = do
    putStrLn "int2bin 9"
    putStrLn . show $ int2bin 9

    putStrLn "chop8 [1, 2, 3, 4, 5, 6, 7, 8, 9]"
    putStrLn . show $ chop8 [1, 2, 3, 4, 5, 6, 7, 8, 9]

    putStrLn "chop8' [1, 2, 3, 4, 5, 6, 7, 8, 9]"
    putStrLn . show $ chop8' [1, 2, 3, 4, 5, 6, 7, 8, 9]

unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold predicate headFunction tailFunction x
    | predicate x = []
    | otherwise = headFunction x : unfold predicate headFunction tailFunction (tailFunction x)

int2bin :: Int -> [Int]
int2bin = unfold (== 0) (`mod` 2) (`div` 2)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = (take 8 bits) : (chop8 (drop 8 bits))

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)