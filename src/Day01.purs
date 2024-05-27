module Day01 (day01) where

import Prelude

import Control.Alt ((<|>))
import Data.Array (reverse)
import Data.Either (hush)
import Data.Foldable (sum)
import Data.Maybe (Maybe)
import Data.String.Utils (lines, trimEnd)
import Data.String.CodePoints (toCodePointArray, fromCodePointArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Parsing (Parser, runParser)
import Parsing.Combinators (try)
import Parsing.String (string, char, anyTill)

day01 :: String -> Maybe Int
day01 input = sum <$> extractCallibrationValues input

extractCallibrationValues :: String -> Maybe (Array Int)
extractCallibrationValues = trimEnd >>> lines >>> traverse go
  where
  go input = do
    let reversed = fromCodePointArray <<< reverse <<< toCodePointArray $ input
    fst <- hush $ runParser input (parseDigit parseSpelledDigit)
    lst <- hush $ runParser reversed (parseDigit parseReverseSpelledDigit)
    pure $ (fst * 10) + lst

parseDigit :: Parser String Int -> Parser String Int
parseDigit spelledParser = do
  Tuple _ b <- anyTill $ parseSingleNumericDigit <|> spelledParser
  pure b

parseSingleNumericDigit :: Parser String Int
parseSingleNumericDigit =
  let
    f c i = char c *> pure i
  in
    (f '1' 1)
      <|> (f '2' 2)
      <|> (f '3' 3)
      <|> (f '4' 4)
      <|> (f '5' 5)
      <|> (f '6' 6)
      <|> (f '7' 7)
      <|> (f '8' 8)
      <|> (f '9' 9)

parseSpelledDigit :: Parser String Int
parseSpelledDigit = try $ t <|> f <|> s <|> one <|> eight <|> nine
  where
  t = char 't' *> (string "wo" *> pure 2 <|> string "hree" *> pure 3)
  f = char 'f' *> (string "our" *> pure 4 <|> string "ive" *> pure 5)
  s = char 's' *> (string "ix" *> pure 6 <|> string "even" *> pure 7)
  one = string "one" *> pure 1
  eight = string "eight" *> pure 8
  nine = string "nine" *> pure 9

parseReverseSpelledDigit :: Parser String Int
parseReverseSpelledDigit = try $ e <|> two <|> four <|> six <|> seven <|> eight
  where
  e = char 'e' *> (n <|> string "erht" *> pure 3 <|> string "vif" *> pure 5)
  n = char 'n' *> (char 'o' *> pure 1 <|> string "in" *> pure 9)
  two = string "owt" *> pure 2
  four = string "ruof" *> pure 4
  six = string "xis" *> pure 6
  seven = string "neves" *> pure 7
  eight = string "thgie" *> pure 8
