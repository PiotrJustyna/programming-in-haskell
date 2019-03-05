-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter5_exercise4
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "replicate' 3 True"
    putStrLn . show $ replicate' 3 True

replicate' :: Show a => Int -> a -> [a]
replicate' x y = [y | _ <- [1 .. x]]