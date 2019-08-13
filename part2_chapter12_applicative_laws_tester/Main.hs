-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_applicative_laws_tester
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Control.Applicative

main = do
    putStrLn "Applicative law 1:"
    putStrLn "(pure id) <*> x = x"
    putStrLn "---"
    
    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ (pure id) <*> listOfWords

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn . show $ (pure id) <*> maybeAWord

    putStrLn ""
    putStrLn "Applicative law 2:"
    putStrLn "pure (g x) = (pure g) <*> (pure x)"
    putStrLn "---"
    
    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ ((pure (length listOfWords)) :: [Int])
    putStrLn . show $ (pure length) <*> ((pure listOfWords) :: [[String]])

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn . show $ ((pure (customLength maybeAWord)) :: (Maybe Int))
    putStrLn . show $ (pure customLength) <*> ((pure maybeAWord) :: (Maybe (Maybe String)))

    putStrLn ""
    putStrLn "Applicative law 3:"
    putStrLn "x <*> (pure y) = pure (\\g -> g y) <*> x"
    putStrLn "---"
    
    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ x <*> (pure "Hello!")
    putStrLn . show $ (pure (\z -> z "Hello!") <*> x)

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn . show $ x' <*> (pure "Hello!")
    putStrLn . show $ (pure (\z -> z "Hello!") <*> x')

listOfWords :: [String]
listOfWords = ["Hello World!"]

maybeAWord :: Maybe String
maybeAWord = Just "Hello World!"

customLength :: (Maybe String) -> Int
customLength (Just x) = length x
customLength Nothing = 0

x :: Maybe (a -> a)
x = Just id

x' :: [String -> String]
x' = [reverse, reverse]