-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter6_exercise4
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "euclid 6 27"
    putStrLn . show $ euclid 6 27

    putStrLn "euclid 192 270"
    putStrLn . show $ euclid 192 270

euclid :: Int -> Int -> Int
euclid x y
    | x < 0 || y < 0 = error "Negative integers are not expected."
    | x == 0 = y
    | y == 0 = x
    | otherwise = euclid b r
        where
            a = if xGreaterOrEqualToY then x else y
            b = if xGreaterOrEqualToY then y else x
            xGreaterOrEqualToY = x >= y
            q = a `div` b
            r = a `mod` b