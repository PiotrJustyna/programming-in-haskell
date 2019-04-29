-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter9_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "removeFirstOccurence 1 list1"
    putStrLn . show $ removeFirstOccurence 1 list1

    putStrLn "isChoice list2 list1"
    putStrLn . show $ isChoice list2 list1

    putStrLn "isChoice list3 list1"
    putStrLn . show $ isChoice list3 list1

    putStrLn "isChoice list4 list1"
    putStrLn . show $ isChoice list4 list1

list1 :: [Int]
list1 = [1, 2, 3]

list2 :: [Int]
list2 = [1, 2]

list3 :: [Int]
list3 = list1

list4 :: [Int]
list4 = [1, 2, 3 ,4]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice baseList (y:ys) =
    (elem y baseList) &&
    (isChoice (removeFirstOccurence y baseList) ys)

removeFirstOccurence :: Eq a => a -> [a] -> [a]
removeFirstOccurence x (y:ys) =
    if (x == y)
        then ys
        else y:(removeFirstOccurence x ys)