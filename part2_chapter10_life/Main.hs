-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter10_life
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    clear
    life glider

type Position = (Int, Int)

type Board = [Position]

width :: Int
width = 10

height :: Int
height = 10

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]

clear :: IO ()
clear = putStr "\ESC[2J"

writeAt :: Position -> String -> IO ()
writeAt position text = do
    goto position
    putStr text

goto :: Position -> IO ()
goto (x, y) = putStr ("\ESC[" ++ (show y) ++ ";" ++ (show x) ++ "H")

showCells :: Board -> IO ()
showCells board = sequence_ [writeAt singlePosition "0" | singlePosition <- board]

isAlive :: Board -> Position -> Bool
isAlive board position = elem position board

isEmpty :: Board -> Position -> Bool
isEmpty board position = not (isAlive board position)

neighbors :: Position -> [Position]
neighbors (x, y) = 
    [(x - 1, y - 1),
    (x, y - 1),
    (x + 1, y - 1),
    (x - 1, y),
    (x + 1, y),
    (x - 1, y + 1),
    (x, y + 1),
    (x + 1, y + 1)]

wrap :: Position -> Position
wrap (x, y) =
    (((x - 1) `mod` width) + 1,
    ((y - 1) `mod` height) + 1)

numberOfLiveNeighbors :: Board -> Position -> Int
numberOfLiveNeighbors board = length . filter (isAlive board) . neighbors

survivors :: Board -> [Position]
survivors board =
    [singlePosition |
    singlePosition <- board,
    elem (numberOfLiveNeighbors board singlePosition) [2, 3]]

births :: Board -> [Position]
births board =
    [singlePosition |
    singlePosition <- removeDuplicates (concat (map neighbors board)),
    isEmpty board singlePosition,
    numberOfLiveNeighbors board singlePosition == 3]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x:filter (/= x) xs

nextGeneration :: Board -> Board
nextGeneration board = (survivors board) ++ (births board)

life :: Board -> IO ()
life board = do
    clear
    showCells board
    a <- getChar
    life (nextGeneration board)

wait :: Int -> IO ()
wait x = sequence_ [return () | _ <- [1 .. x]]