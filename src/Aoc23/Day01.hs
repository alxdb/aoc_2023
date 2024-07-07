module Aoc23.Day01 (solution) where

import Flow
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
extractCalibrationValue = parse calibrationValueParser .> first show

calibrationValueParser :: ParserC Int
calibrationValueParser = do
  digits <- some <| next <| (digitParser <|> spelledDigitSubstrings)
  return $ head digits * 10 + last digits
 where
  spelledDigitSubstrings = lookAhead spelledDigitParser <* anything

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
