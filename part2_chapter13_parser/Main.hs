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

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- (>>=) :: (Parser a) -> (a -> Parser b) -> (Parser b)
    (>>=) parser functionToParser = P (\input -> case parse parser input of
        [] -> []
        [(value, out)] -> parse (functionToParser value) out)

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

item :: Parser Char
item = P (\inp -> case inp of
    [] -> []
    (x:xs) -> [(x, xs)])

three :: Parser (Char, Char)
three = pure g <*> item <*> item <*> item
    where
        g x y z = (x, z)

lengthParser :: Parser (String -> Int)
lengthParser = P (\inp -> [(length, inp)])

crappyParser :: Parser String
crappyParser = P (\inp -> [(inp, inp)])

uselessParser :: Char -> (Parser String)
uselessParser x = P (\inp -> [((x:inp), inp)])

main = do
    putStrLn "Parser:"
    putStrLn . show $ parse item ""
    putStrLn . show $ parse item "abc"

    putStrLn "Functor:"
    putStrLn . show $ parse (toUpper <$> item) ""
    putStrLn . show $ parse (toUpper <$> item) "abc"

    putStrLn "Applicative:"
    putStrLn . show $ parse (pure '1') "abc"
    putStrLn . show $ parse (lengthParser <*> crappyParser) "abcdef"
    putStrLn . show $ parse three "abcdef"

    putStrLn "Monad:"
    putStrLn . show $ parse (return '1') "abc"
    putStrLn . show $ parse (item >>= (\x -> crappyParser)) "abcdef"
    putStrLn . show $ parse (item >>= uselessParser) "abcdef"