module Core.Parser (Error (..), Parser (..), ParserResult, many, satisfy, eoi, parse) where

import Control.Applicative
import Control.Monad
import Data.Bifoldable
import Data.Bifunctor
import Data.Bool
import Data.Either
import Data.Eq
import Data.Function
import Data.Semigroup ((<>))
import Data.Tuple
import GHC.Base (undefined)
import Text.Show

data Error t where
  Unexpected :: t -> Error t
  Empty :: Error t
  deriving (Eq, Show)

type ParserRes t a = Either [Error t] (a, [t])

type ParserRun t a = [t] -> ParserRes t a

type ParserResult t a = Either [Error t] a

newtype Parser t a = Parser {run :: ParserRun t a}

parse :: Parser t a -> [t] -> ParserResult t a
parse p ts = fst <$> run p ts

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
  fmap f p = Parser (fmap (first f) . run p)

instance Applicative (Parser t) where
  pure :: a -> Parser t a
  pure x = Parser (\rest -> Right (x, rest))

  (<*>) :: Parser t (a -> b) -> Parser t a -> Parser t b
  pf <*> pa = Parser go
    where
      go r0 = do
        (f, r1) <- run pf r0
        (a, r2) <- run pa r1
        return (f a, r2)

instance Monad (Parser t) where
  return = pure

  (>>=) :: Parser t a -> (a -> Parser t b) -> Parser t b
  p >>= f = Parser go
    where
      go r0 = do
        (a, r1) <- run p r0
        run (f a) r1

instance Alternative (Parser t) where
  empty :: Parser t a
  empty = Parser (\_ -> Left [Empty])

  (<|>) :: Parser t a -> Parser t a -> Parser t a
  pl <|> pr = Parser go
    where
      go r0 = case run pl r0 of
        Right x -> Right x
        Left el -> case run pr r0 of
          Right x -> Right x
          Left er -> Left $ el <> er

-- go :: ParserRun t a
-- go r = bifoldrM _ _ r (run pl r)

-- go r = either (\el -> either (\er -> Left $ el <> er) pure (run pr r)) pure (run pl r)

-- many :: Parser t b -> Parser t [b]
-- many p = Parser go
--   where
--     go [] = Left [Empty]
--     go (x : _) = _

-- anyTill :: Parser t b -> Parser t a -> Parser t a
-- anyTill a b = Parser go
--   where
--     go r0 =
