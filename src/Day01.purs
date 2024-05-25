module Day01 where

import Prelude ((>>>))

import Data.Maybe (Maybe)
import Data.Foldable (findMap)
import Data.String.CodePoints (toCodePointArray)
import Data.CodePoint.Unicode (decDigitToInt)


extractCallibrationValue :: String -> Maybe Int
extractCallibrationValue = toCodePointArray >>> findMap decDigitToInt

extractCallibrationValues :: String -> Array Int
extractCallibrationValues _ = []
