module Parser where

import Control.Applicative
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List
import Text.Read

newtype Parser a t = Parser {runParser :: [a] -> Maybe (t, [a])}

satisfy :: (Eq a) => (a -> Bool) -> Parser a a
satisfy predicate = Parser func
  where
    func [] = Nothing -- fail on empty input
    func (x : xs)
      | predicate x = Just (x, xs)
      | otherwise = Nothing

prefix str = Parser func
  where
    func x = if isPrefixOf str x then Just (take (length str) x, drop (length str) x) else Nothing

letter :: Parser Char Char
letter = satisfy isAlpha

char :: Char -> Parser Char Char
char = satisfy . (==)

anyOf :: (Eq a, Foldable t) => t a -> Parser a a
anyOf lst = satisfy $ flip elem lst


first :: (a -> b) -> Parser t a -> Parser t b
first mapper (Parser runParserInstance1) = Parser runParserInstance2
  where
    runParserInstance2 inputString = case runParserInstance1 inputString of
      Nothing -> Nothing
      Just (x, xs) -> Just (mapper x, xs)

instance Functor (Parser a) where
  fmap = first

instance Alternative (Parser a) where
  empty = Parser (const Nothing)
  (<|>) (Parser f1) (Parser f2) = Parser f3
    where
      f3 inputString = f1 inputString <|> f2 inputString

pureParser :: a -> Parser t a
pureParser val = Parser (\x -> Just (val, x))

parserApply :: (a -> b -> c) -> Parser t a -> Parser t b -> Parser t c
parserApply mapper (Parser f1) (Parser f2) = Parser f3
  where
    f3 inputString = do
      (val1, rest1) <- f1 inputString
      (val2, rest2) <- f2 rest1
      pure (mapper val1 val2, rest2)

instance Applicative (Parser a) where
  pure = pureParser
  (<*>) = parserApply id

zeroOrMore :: Parser t a -> Parser t [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser t a -> Parser t [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

terminate :: Parser Char String
terminate = Parser func
  where
    func inp
      | null inp = Just ("", "")
      | isSpace (head inp) = Just ("", inp)
      | otherwise = Nothing


try :: Parser t a -> Parser t (Maybe a)
try p = Parser func
  where
    func inp = case runParser p inp of
      Nothing -> Just (Nothing, inp)
      Just (x, xs) -> Just (Just x, xs)


skip :: Parser t (Maybe a)
skip = Parser f where f input = Just (Nothing, input)

eatWhitespace :: Parser Char String
eatWhitespace = zeroOrMore (satisfy isSpace)


-- This implementation of bind for parsers doesnt consume more input, but instead transforms an existing valid input.
-- TODO figure out if another bind implementation makes more sense than this one
parserBind :: Parser t a -> (a -> Parser t b) -> Parser t b
parserBind (Parser run1) fn = Parser $ \str -> run1 str >>= \(x, xs) -> runParser (fn x) xs

instance Monad (Parser a) where
  return x = Parser func where func y = Just (x, y)
  (>>=) = parserBind