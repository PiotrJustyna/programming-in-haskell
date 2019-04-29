-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part1_chapter10_exercise2
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "initial:"
    putStrLn . show $ initial

    putStrLn "putBoard initial:"
    putBoard initial

    putStrLn "putBoard' initial:"
    putBoard' initial

initial :: Board
initial = [5, 4, 3, 2, 1]

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row numberOfStars = do
    putStr (show row)
    putStr ": "
    putStrLn (concat (replicate numberOfStars "* "))

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow index value | (index, value) <- zip [1 ..] board]

putBoard' :: Board -> IO ()
putBoard' board = putBoardHelper board 1

putBoardHelper :: Board -> Int -> IO ()
putBoardHelper board currentRowNumber = do
    putRow currentRowNumber (board !! currentRowIndex)
    if currentRowNumber < (length board)
        then
            putBoardHelper board (currentRowNumber + 1)
        else
            return ()
    where
        currentRowIndex = currentRowNumber - 1
