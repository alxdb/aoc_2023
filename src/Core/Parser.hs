module Core.Parser (
  Error (..),
  Parser (..),
  ParserResult,
  satisfy,
  end,
  parseWhile,
  parse,
  parseShow,
  lookAhead,
) where

import Prelude

import Control.Applicative (Alternative (..))

import Control.Error (fmapL)

import Data.Containers.ListUtils (nubOrd)
import Data.EitherR (EitherR (..))

data Error t where
  Unexpected :: t -> Error t
  Custom :: String -> Error t
  Empty :: Error t
  deriving (Eq, Show, Ord)

type ParserRes t a = Either [Error t] (a, [t])

type ParserRun t a = [t] -> ParserRes t a

type ParserResult t a = Either [Error t] a

newtype Parser t a = Parser {runParser :: ParserRun t a} deriving (Functor)

parse :: Parser t a -> [t] -> ParserResult t a
parse p ts = fst <$> runParser p ts

parseShow :: (Show t) => Parser t a -> [t] -> Either String a
parseShow p ts = fmapL show $ parse p ts

satisfy :: (t -> Bool) -> Parser t t
satisfy predicate = Parser go
 where
  go [] = Left [Empty]
  go (x : xs)
    | predicate x = Right (x, xs)
    | otherwise = Left [Unexpected x]

lookAhead :: Parser t a -> Parser t a
lookAhead p = Parser go
 where
  go r = do
    (a, _) <- runParser p r
    return (a, r)

parseWhile :: (t -> Bool) -> Parser t a -> Parser t a
parseWhile f p = Parser go
 where
  go r = do
    let (r1, r2) = span f r
    (a, _) <- runParser p r1
    return (a, r2)

end :: Parser t ()
end = Parser go
 where
  go [] = Right ((), [])
  go (x : _) = Left [Unexpected x]

instance Applicative (Parser t) where
  pure x = Parser (\rest -> Right (x, rest))

  pf <*> pa = Parser go
   where
    go r0 = do
      (f, r1) <- runParser pf r0
      (a, r2) <- runParser pa r1
      return (f a, r2)

instance Monad (Parser t) where
  return = pure

  p >>= f = Parser go
   where
    go r0 = do
      (a, r1) <- runParser p r0
      runParser (f a) r1

instance MonadFail (Parser t) where
  fail msg = Parser (\_ -> Left [Custom msg])

instance (Ord t) => Alternative (Parser t) where
  empty = Parser (\_ -> Left [Empty])

  pl <|> pr = Parser go
   where
    go r = runEitherR $ do
      el <- EitherR (runParser pl r)
      er <- EitherR (runParser pr r)
      return . nubOrd $ el <> er
