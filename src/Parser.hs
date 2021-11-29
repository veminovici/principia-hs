{-# LANGUAGE LambdaCase #-}

-- http://dev.stephendiehl.com/fun/002_parsers.html#parsing

module Parser where

import Data.Char
import Data.Bifunctor;
import Control.Monad
import Control.Applicative
import GHC.Base (MonadPlus, Alternative)

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser p s =
    case parse p s of
        [(res, [])] -> res
        [(_, rs)] -> error "Parser didn't consume the whole stream"
        _ -> error "Parser error"

--
-- Basic functionality
--

item :: (Char -> a) -> Parser a
item f = Parser $ \case
    [] -> []
    (c:cs) -> [(f c, cs)]

unit :: a -> Parser a
unit a = Parser $ \s ->
    [(a, s)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind m f = Parser $ \s ->
    let xs = parse m s in concatMap (\(a, s1) -> parse (f a) s1) xs

failure :: Parser a
failure = Parser $ const []

combine :: Parser a -> Parser a -> Parser a
combine a b = Parser $ \s ->
    parse a s ++ parse b s

option :: Parser a -> Parser a -> Parser a
option a b = Parser $ \s ->
    case parse a s of
        [] -> parse b s
        res -> res

--
-- Functor, Applicative, Monad
--

instance Functor Parser where
    fmap f (Parser m) = Parser $ \s ->
        [(f a, s) | (a, s) <- m s]

instance Applicative Parser where
    pure = unit
    (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
        [(f a, s2)| (f, s1) <- cs1 s, (a, s2) <- cs2 s1]

instance Monad Parser where
    return = unit
    (>>=) = bind

--
-- Alternative, MonadPlus
--

instance Alternative Parser where
    empty = failure
    (<|>) = option

instance MonadPlus Parser where
    mzero = failure
    mplus = combine

--
-- Higher level function
--

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item id `bind` (\c ->
    if f c
    then unit c
    else failure )

oneOf :: [Char] -> Parser Char
oneOf cs = satisfy (`elem` cs)

-- | Returns a parser which parses the stream only if the head is a given character.
char :: Char -> Parser Char
char c = satisfy (c ==)

-- | A parser which reads a sequence of digits
natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

-- || Returns a parser which reads a given string
string :: String -> Parser String 
string [] = return []
string (c : cs) = do { char c; string cs; return (c: cs) }

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved = token . string

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int 
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
    reserved "("
    n <- m
    reserved ")"
    return n
