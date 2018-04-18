{-# LANGUAGE InstanceSigs #-}
module Block3 where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Maybe
-- -------------------------- TASK 1 ---------------------------------

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f r = Parser (fmap (first f) . runParser r)


instance Applicative (Parser s) where
  pure :: x -> Parser s x
  pure x = Parser $ \s -> Just (x, s)
  (<*>) :: Parser s (x -> y) -> Parser s x -> Parser s y
  (<*>) a1  a2 = Parser
      (runParser a1 >=>
         (\ (f, str1) ->
            runParser a2 str1 >>= \ (x, str2) -> Just (f x, str2)))

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (Parser a1) <|> (Parser a2) = Parser $ \s -> a1 s <|> a2 s

instance Monad (Parser s) where
  return = pure
  (>>=) :: Parser s x -> (x -> Parser s y) -> Parser s y
  p >>= f = Parser (runParser p >=> (\(x, sx) -> runParser (f x) sx))


-- -------------------------- TASK 2 ---------------------------------

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

eof :: Parser s ()
eof = Parser $ \s -> case s of
    [] -> Just ((), s)
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser f
  where
  f [] = Nothing
  f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char Char
char c = satisfy (== c)

element :: Eq s => s -> Parser s s
element x = satisfy (== x)

stream :: Eq s => [s] -> Parser s [s]
stream x = Parser $ \s -> case x of
    (a:b) -> runParser (element a) s >>= \(a, t) -> runParser (stream b) t >>= \(n, f) -> Just(a : n, f)
    [] -> Just([], s)

-- -------------------------- TASK 3 ---------------------------------
spaces :: Parser Char String
spaces = many $ satisfy isSpace

integer :: Parser Char Integer
integer = Parser (fmap (first read) . runParser (some (satisfy isDigit)))

sign :: Num p => Char -> p -> p
sign s v = if s == '-' then -v else v

onlySignedInteger :: Parser Char Integer
onlySignedInteger =  sign <$> (element '+' <|> element '-') <*> integer

notOnlySignedInteger :: Parser Char Integer
notOnlySignedInteger =  sign <$> (element '+' <|> element '-' <|> pure ' ') <*> integer


matchingParenthesis :: Map Char Char
matchingParenthesis = Map.fromList [
    ('(', ')')
  , ('{', '}')
  , ('[', ']')
  ]

isOpening :: Char -> Bool
isOpening c = Data.Maybe.isJust $ Map.lookup c matchingParenthesis


balanced :: Parser Char ()
balanced = Parser $ \s -> let t = balanced' [] s in
            if t then  Just((), s) else Nothing
-- balances =   stream (balanced' [])

balanced' :: String -> String -> Bool
balanced' a b = case (a, b) of
     ([], "" )    -> True
     (_, "" )     -> False
     ([], c:cs)  -> balanced' [c] cs
     (o:os, c:cs) -> if isOpening c then balanced' (c:o:os) cs
                         else case Map.lookup o matchingParenthesis of
                                Nothing      -> False
                                Just closing -> (closing == c) && balanced' os cs

-- -------------------------- TASK 4 ---------------------------------

spacedInteger :: Parser Char Integer
spacedInteger = spaces *> notOnlySignedInteger <* spaces

commaSpacedInteger :: Parser Char Integer
commaSpacedInteger = element ',' *> spacedInteger

num 0 _    = Parser $ \s -> Just ([], s)
num n elem = (:) <$> elem <*> num (n - 1) elem

parseListOfLists :: Parser Char [[Integer]]
parseListOfLists = let l = spacedInteger >>= \n -> num n commaSpacedInteger in
    (:) <$> l <*> many (element ',' *> l)
