module Aoc23.Day01 (solution) where

import Prelude

import Control.Applicative

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator
import Data.Bifunctor (Bifunctor (first))

solution :: Solution
solution = sumLines extractCalibrationValue

extractCalibrationValue :: String -> Either String Int
extractCalibrationValue = first show . parse calibrationValueParser

calibrationValueParser :: ParserC Int
calibrationValueParser = do
  digits <- some . next $ (digitParser <|> (const <$> lookAhead spelledDigitParser <*> anything))
  return $ head digits * 10 + last digits

spelledDigitParser :: ParserC Int
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
