module Block3 where

import           Control.Applicative
import           Data.List
import           Data.Maybe
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.Map            (Map)
import qualified Data.Map            as Map
-- -------------------------- TASK 1 ---------------------------------

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
  fmap f r = Parser (fmap (first f) . runParser r)


instance Applicative (Parser s) where
  pure x = Parser $ \s -> Just (x, s)
  (<*>) a1  a2 = Parser
      (runParser a1 >=>
         (\ (f, str1) ->
            runParser a2 str1 >>= \ (x, str2) -> Just (f x, str2)))

instance Alternative (Parser s) where
  empty = Parser $ const Nothing
  (Parser a1) <|> (Parser a2) = Parser $ \s -> a1 s <|> a2 s

instance Monad (Parser s) where
  return = pure
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

sign s v = if s == '-' then -v else v

onlySignedInteger :: Parser Char Integer
onlySignedInteger =  sign <$> (element '+' <|> element '-') <*> integer

notOnlySignedInteger :: Parser Char Integer
notOnlySignedInteger =  sign <$> (element '+' <|> element '-' <|> pure ' ') <*> integer


matchingParens :: Map Char Char
matchingParens = Map.fromList [
    ('(', ')')
  , ('{', '}')
  , ('[', ']')
  ]

isOpening :: Char -> Bool
isOpening c = Data.Maybe.isJust $ Map.lookup c matchingParens

balanced = Parser $ \s -> let t = balanced' [] s in
            if t then  Just((), s) else Nothing

balanced' :: String -> String -> Bool
balanced' [] ""     = True
balanced' _  ""     = False
balanced' [] (c:cs) = balanced' [c] cs
balanced' (o:os) (c:cs)
  | isOpening c = balanced' (c:o:os) cs
  | otherwise   = case Map.lookup o matchingParens of
      Nothing -> False
--       Just (closing == c) && balanced' os cs
      Just closing -> if closing == c
        then balanced' os cs
        else False

-- -------------------------- TASK 4 ---------------------------------

spacedInteger :: Parser Char Integer
spacedInteger = spaces *> notOnlySignedInteger <* spaces

commaSpacedInteger :: Parser Char Integer
commaSpacedInteger = element ',' *> spacedInteger

num 0 _      = Parser $ \s -> Just ([], s)
num n elem = (:) <$> elem <*> num (n - 1) elem

parseListOfLists :: Parser Char [[Integer]]
parseListOfLists = let l = spacedInteger >>= \n -> num n commaSpacedInteger in
    (:) <$> l <*> many (element ',' *> l)
