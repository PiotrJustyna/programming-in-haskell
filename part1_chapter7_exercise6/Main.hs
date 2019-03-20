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

    putStrLn "map (+1) [1, 2, 3]"
    putStrLn . show $ map (+1) [1, 2, 3]

    putStrLn "map' (+1) [1, 2, 3]"
    putStrLn . show $ map' (+1) [1, 2, 3]

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

-- This one was quite confusing to me, so I thought I would leave some comments to complement the implementation.
-- The author asks readers to implement "map f". "map" signature is:
--
-- map :: (a -> b) -> [a] -> [b]
--
-- which translates to (for lists, that is): every item in list [a] gets mapped from type a to type b and then
-- returned as [b].
--
-- Now with that in mind, does it really makes sense to apply "unfold" here? Is the function not supposed to build a list
-- using the starting value and the functions provided? If not, I'm not sure what really is required. If so, though,
-- the type signature defined by the regular "map" is going to make:
-- the implementation unnecessarily complicated
-- map and map' are not going to be interchangable.
--
-- My initial thought was to implement function:
--
-- map' :: (a -> b) -> [a] -> [b]
--
-- with the signature identical to the regular "map" (just a different implementation) so that both can be used interchangeably.
-- It took me a while before I realized that it is probably more tricky than it should be.
-- This is when I started looking for other solutions, just to get an idea how people interpret the task.
-- What I found mademe even more confused. Examples:
--
-- 1. https://github.com/evturn's implementaion: https://github.com/evturn/programming-in-haskell/blob/master/07-higher-order-functions/07.9-exercises.hs
-- 
-- map' p f xs = unfold p f f xs
--
-- the type here is:
--
-- map' :: (a -> Bool) -> (a -> a) -> a -> [a]
--
-- So as you can see, it doesn't match the original "map"'s type and the input argument (second last "a") gets "unfolded" into [a].
--
-- 2. Another example I found is the one by https://github.com/RoccoMathijn: https://github.com/RoccoMathijn/programming-in-haskell/blob/master/chapter07.hs
--
-- map' :: (a -> b) -> [a] -> [b]
-- map' f = unfold (null) (f.head) (tail)
--
-- But this again is not intercangeable with the original "map" as b is simply [a], so the result is really [[a]].
--
-- After more time than I'd like to admit, I finally figured out the implementation which is interchangeable with the original "map".
-- Terribly inefficient, but I believe it answers the exercise's question precisely.

map' :: (a -> b) -> [a] -> [b]
map' f = concat . unfold null (listify . f . head) (tail)

listify :: a -> [a]
listify x = [x]