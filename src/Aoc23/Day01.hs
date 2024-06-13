module Aoc23.Day01 (solution) where

import Prelude

import Control.Applicative
import Data.Char
import Data.Functor (($>))

import Aoc23.Solution
import Core.Parser

import Data.EitherR (fmapL)

solution :: Solution
solution = sumLines extractCalibrationValue

extractCalibrationValue :: String -> Either String Int
extractCalibrationValue = fmapL errorMessage . parse calibrationValueParser
 where
  errorMessage [Empty] = "Couldn't find a digit in input line"
  errorMessage _ = undefined

calibrationValueParser :: Parser Char Int
calibrationValueParser = do
  ds <- some . next $ digitParser
  return $ head ds * 10 + last ds

digitParser :: Parser Char Int
digitParser =
  digitToInt
    <$> satisfy isDigit
    <|> do
      -- Parses portmenteaus of digits
      s <- lookAhead spelledDigitParser
      _ <- anything
      return s

spelledDigitParser :: Parser Char Int
spelledDigitParser =
  asum . map (\(s, x) -> exact s $> x) $
    [ ("one", 1)
    , ("two", 2)
    , ("three", 3)
    , ("four", 4)
    , ("five", 5)
    , ("six", 6)
    , ("seven", 7)
    , ("eight", 8)
    , ("nine", 9)
    ]
