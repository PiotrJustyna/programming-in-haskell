-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter7_exercise6
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "int2bin 9"
    putStrLn . show $ int2bin 9

unfold :: (a -> Bool) -> (a -> a) -> (a -> a) -> a -> [a]
unfold predicate headFunction tailFunction x
    | predicate x = []
    | otherwise = headFunction x : unfold predicate headFunction tailFunction (tailFunction x)

int2bin = unfold (== 0) (`mod` 2) (`div` 2)