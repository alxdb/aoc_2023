module Aoc23.Day01 (solution) where

import Data.Char (digitToInt)
import Data.Text (Text, lines, null)
import Text.Parsec qualified as PS
import Prelude (Either, Int, filter, last, map, mapM, not, sum, (*), (+), (.), (<$>), (>>))

solution :: Text -> Either PS.ParseError Int
solution = (sum <$>) . mapM parseDigits . filter (not . null) . lines

parseDigits :: Text -> Either PS.ParseError Int
parseDigits = PS.runParser p () ""
  where
    p = getDigit . map digitToInt <$> PS.many (PS.between letters letters PS.digit)

    letters = PS.skipMany PS.letter

    getDigit [] = 0
    getDigit [d1] = (d1 * 10) + d1
    getDigit (d1 : ds) = (d1 * 10) + last ds
