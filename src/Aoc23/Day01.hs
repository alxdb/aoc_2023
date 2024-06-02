module Aoc23.Day01 (solution) where

import Core.Parser
import Prelude (Char, Either (..), Int, String, undefined)

solution :: String -> ParserResult Char Int
solution _ = Right 142

-- solution = (sum <$>) . mapM parseDigits . filter (not . null) . lines

parseDigits :: String -> ParserResult Char Int
parseDigits = parse p
  where
    p = undefined

-- p = getDigit . map digitToInt <$> many (PS.between letters letters PS.digit)
--
-- letters = PS.skipMany PS.letter
--
-- getDigit [] = 0
-- getDigit [d1] = (d1 * 10) + d1
-- getDigit (d1 : ds) = (d1 * 10) + last ds
