module Core.Parser.Combinator (
  anything,
  exactly,
  exact,
  never,
  next,
  mapping,
  sepBySome,
  sepByMany,
  endBySome,
  endByMany,
  surroundedBy,
  notEnd,
) where

import Prelude

import Control.Applicative (Alternative (..), asum)
import Data.Functor (void, ($>))

import Core.Parser

anything :: Parser t t
anything = satisfy (const True)

exactly :: (Eq t) => t -> Parser t t
exactly x = satisfy (== x)

exact :: (Eq t) => [t] -> Parser t [t]
exact = mapM exactly

never :: (Eq t) => t -> Parser t t
never x = satisfy (/= x)

next :: (Ord t) => Parser t a -> Parser t a
next p = do
  v <- (Just <$> p) <|> (Nothing <$ anything)
  maybe (next p) return v

mapping :: (Ord t) => [(Parser t a, b)] -> Parser t b
mapping = asum . map (uncurry ($>))

sepBySome :: (Ord t) => Parser t a -> Parser t b -> Parser t [a]
sepBySome p s = (:) <$> p <*> many (s >> p)

sepByMany :: (Ord t) => Parser t a -> Parser t b -> Parser t [a]
sepByMany p s = sepBySome p s <|> pure []

endBySome :: (Ord t) => Parser t a -> Parser t b -> Parser t c -> Parser t [a]
endBySome p s e = sepBySome p s <* e

endByMany :: (Ord t) => Parser t a -> Parser t b -> Parser t c -> Parser t [a]
endByMany p s e = sepByMany p s <* e

surroundedBy :: (Ord t) => Parser t a -> Parser t b -> Parser t b
surroundedBy s p = s *> p <* s

notEnd :: Parser t ()
notEnd = void (lookAhead anything)
