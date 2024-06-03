module Core.Parser (Error (..), Parser (..), ParserResult, many, satisfy, eoi, parse) where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bool
import Data.Either
import Data.EitherR
import Data.Eq
import Data.Function
import Data.Semigroup ((<>))
import Data.Tuple
import Text.Show

data Error t where
  Unexpected :: t -> Error t
  Empty :: Error t
  deriving (Eq, Show)

type ParserRes t a = Either [Error t] (a, [t])

type ParserRun t a = [t] -> ParserRes t a

type ParserResult t a = Either [Error t] a

newtype Parser t a = Parser {runParser :: ParserRun t a}

parse :: Parser t a -> [t] -> ParserResult t a
parse p ts = fst <$> runParser p ts

satisfy :: (t -> Bool) -> Parser t t
satisfy predicate = Parser go
  where
    go [] = Left [Empty]
    go (x : xs)
      | predicate x = Right (x, xs)
      | otherwise = Left [Unexpected x]

eoi :: Parser t ()
eoi = Parser go
  where
    go [] = Right ((), [])
    go (x : _) = Left [Unexpected x]

instance Functor (Parser t) where
  fmap f p = Parser (fmap (first f) . runParser p)

instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure x = Parser (\rest -> Right (x, rest))

  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  pf <*> pa = Parser go
    where
      go r0 = do
        (f, r1) <- runParser pf r0
        (a, r2) <- runParser pa r1
        return (f a, r2)

instance Monad (Parser t) where
  return = pure

  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  p >>= f = Parser go
    where
      go r0 = do
        (a, r1) <- runParser p r0
        runParser (f a) r1

instance Alternative (Parser t) where
  empty :: Parser t a
  empty = Parser (\_ -> Left [Empty])

  (<|>) :: Parser t a -> Parser t a -> Parser t a
  pl <|> pr = Parser go
    where
      go r = runEitherR $ do
        el <- EitherR (runParser pl r)
        er <- EitherR (runParser pr r)
        return $ el <> er
