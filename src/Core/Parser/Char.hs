module Core.Parser.Char (digitParser, intParser) where

import Prelude

import Control.Applicative (some)
import Data.Char (digitToInt, isDigit)

import Core.Parser

digitParser :: Parser Char Int
digitParser = digitToInt <$> satisfy isDigit

intParser :: Parser Char Int
intParser = read <$> some (satisfy isDigit)
