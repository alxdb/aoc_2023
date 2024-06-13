module Core.Parser.Char (digitParser, intParser, exactMapping) where

import Prelude

import Control.Applicative (some)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)

import Core.Parser
import Core.Parser.Combinator

digitParser :: Parser Char Int
digitParser = digitToInt <$> satisfy isDigit

intParser :: Parser Char Int
intParser = read <$> some (satisfy isDigit)

exactMapping :: [(String, a)] -> Parser Char a
exactMapping = mapping . fmap (first exact)
