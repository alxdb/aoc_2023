module Core.Parser.Char (
  digitParser,
  intParser,
  exactMapping,
  lineParser,
  ParserC,
  spaces,
  inSpaces,
) where

import Flow
import Prelude

import Control.Applicative (some)
import Data.Bifunctor (first)
import Data.Char (digitToInt, isDigit)
import Data.Functor

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
lineParser p = sepByMany (parseWhile (/= '\n') p) (exactly '\n' >> notEnd)

spaces :: ParserC ()
spaces = void <| some <| exactly ' '

inSpaces :: ParserC a -> ParserC a
inSpaces = surroundedBy spaces
