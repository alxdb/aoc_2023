module Aoc23.Day01 (solution) where

import Prelude

import Control.Applicative

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

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
  digits <- some . next $ (digitParser <|> (const <$> lookAhead spelledDigitParser <*> anything))
  return $ head digits * 10 + last digits

spelledDigitParser :: Parser Char Int
spelledDigitParser =
  exactMapping
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
