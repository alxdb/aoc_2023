module Day01 (day01, extractCallibrationValues) where

import Prelude

import Data.Array (reverse)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (findMap, sum)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (toCodePointArray)
import Data.String.Utils (lines, trimEnd)
import Data.Traversable (traverse)

day01 :: String -> Maybe Int
day01 input = sum <$> extractCallibrationValues input

extractCallibrationValues :: String -> Maybe (Array Int)
extractCallibrationValues = trimEnd >>> lines >>> traverse go
  where
  go input = do
    let arr = toCodePointArray input
    let findDigit = findMap decDigitToInt
    fst <- findDigit arr
    lst <- findDigit <<< reverse $ arr
    Just $ (fst * 10) + lst
