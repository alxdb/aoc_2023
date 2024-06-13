module Core.Parser.Combinator (anything, exactly, exact, next, mapping) where

import Prelude

import Control.Applicative (Alternative (..), asum)
import Data.Functor (($>))

import Core.Parser

anything :: Parser t t
anything = satisfy (const True)

exactly :: (Eq t) => t -> Parser t t
exactly x = satisfy (== x)

exact :: (Eq t) => [t] -> Parser t [t]
exact = mapM exactly

next :: (Ord t) => Parser t a -> Parser t a
next p = go
 where
  go = do
    v <- (Just <$> p) <|> (Nothing <$ anything)
    maybe go return v

mapping :: (Ord t) => [(Parser t a, b)] -> Parser t b
mapping = asum . map (uncurry ($>))
