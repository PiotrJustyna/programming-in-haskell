-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter12_functor_laws_tester
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
main = do
    putStrLn "Functor law 1:"
    putStrLn "fmap id = id"
    putStrLn "---"
    
    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn . show $ fmap id listOfWords

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn . show $ fmap id maybeAWord

    putStrLn ""

    putStrLn "Functor law 2:"
    putStrLn "fmap (g . h) = fmap g . fmap h"
    putStrLn "---"
    
    putStrLn ""
    putStrLn "Example 1:"
    putStrLn "---"
    putStrLn $ show compositionOfFunctionsExample1
    putStrLn $ show compositionOfFmapsExample1
    putStrLn . show $ compositionOfFunctionsExample1 == compositionOfFmapsExample1

    putStrLn ""
    putStrLn "Example 2:"
    putStrLn "---"
    putStrLn $ show compositionOfFunctionsExample2
    putStrLn $ show compositionOfFmapsExample2
    putStrLn . show $ compositionOfFunctionsExample2 == compositionOfFmapsExample2

    where
        compositionOfFunctionsExample1 = fmap ((* (-1)) . length) listOfWords
        compositionOfFmapsExample1 = ((fmap (* (-1))) . (fmap length)) listOfWords
        compositionOfFunctionsExample2 = fmap ((* (-1)) . length) maybeAWord
        compositionOfFmapsExample2 = ((fmap (* (-1))) . (fmap length)) maybeAWord

listOfWords :: [String]
listOfWords = ["Hello World!"]

maybeAWord :: Maybe String
maybeAWord = Just "Hello World!"