-- option 1 (https://github.com/PiotrJustyna/haskell-anywhere):
-- ./ghci.bat C:\Users\piotr_justyna\Documents\github\programming-in-haskell\part2_chapter13_parser
-- option 2 (stack):
-- stack ghci
-- option 3 (ghci):
-- ghci
--
-- :load Main
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap function parser = P (\input -> case parse parser input of
        [] -> []
        [(value, out)] -> [(function value, out)])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\input -> [(x, input)])

    -- <*> :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) parserFunction parser = P (\input -> case parse parserFunction input of
        [] -> []
        [(function, out)] -> parse (fmap function parser) out)

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)])

main = do
    putStrLn "Parser:"
    putStrLn . show $ parse item ""
    putStrLn . show $ parse item "abc"

    putStrLn "Functor:"
    putStrLn . show $ parse (toUpper <$> item) ""
    putStrLn . show $ parse (toUpper <$> item) "abc"

    putStrLn "Applicative:"
    putStrLn . show $ parse (pure '1') "abc"