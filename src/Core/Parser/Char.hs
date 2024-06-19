module Core.Parser.Char (digitParser, intParser, exactMapping, lineParser, ParserC) where

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

lineParser :: ParserC a -> ParserC [a]
lineParser p = endByMany p (exactly '\n' >> lookAhead anything) (optional (exactly '\n'))
