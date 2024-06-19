module Core.Parser.Char (digitParser, intParser, exactMapping, ParserC) where

import Prelude

import Control.Applicative (some)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)

import Core.Parser
import Core.Parser.Combinator

type ParserC = Parser Char

digitParser :: ParserC Int
digitParser = digitToInt <$> satisfy isDigit

intParser :: ParserC Int
intParser = read <$> some (satisfy isDigit)

exactMapping :: [(String, a)] -> ParserC a
exactMapping = mapping . fmap (first exact)
