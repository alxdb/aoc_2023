module Day01 (day01, extractCallibrationValues) where

import Prelude

import Data.Array (reverse)
import Data.CodePoint.Unicode (decDigitToInt)
import Data.Foldable (findMap, sum)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (toCodePointArray)
import Data.String.Utils (lines, trimEnd)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

day01 :: String -> Effect Unit
day01 inputFile = do
  input <- readTextFile UTF8 inputFile
  case (extractCallibrationValues input) of
    Just calibrationValues -> log <<< show $ sum calibrationValues
    Nothing -> log "Cannot parse input!"

extractCallibrationValues :: String -> Maybe (Array Int)
extractCallibrationValues = trimEnd >>> lines >>> traverse go
  where
  go input = do
    let arr = toCodePointArray input
    let findDigit = findMap decDigitToInt
    fst <- findDigit arr
    lst <- findDigit <<< reverse $ arr
    Just $ (fst * 10) + lst
