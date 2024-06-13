module Core.Parser.Char (digitParser) where

import Data.Char
import Prelude

import Core.Parser

digitParser :: Parser Char Int
digitParser = digitToInt <$> satisfy isDigit
