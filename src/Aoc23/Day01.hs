module Aoc23.Day01 (solution) where

import Aoc23.Solution
import Control.Applicative
import Core.Parser
import Data.Char
import Data.EitherR
import Data.Functor

solution :: Solution
solution = Solution $ (sum <$>) . mapM calibrationValue . filter (not . null) . lines

calibrationValue :: String -> Either String Int
calibrationValue = fmapL errorMessage . parse p
  where
    p = do
      ds <- some . next $ digit
      return $ head ds * 10 + last ds

    errorMessage [Empty] = "Couldn't find a digit in input line"
    errorMessage _ = undefined

digit :: Parser Char Int
digit =
  digitToInt <$> satisfy isDigit <|> do
    -- Parses portmenteaus of digits
    s <- lookAhead spelledDigit
    _ <- anything
    return s

spelledDigit :: Parser Char Int
spelledDigit =
  asum . map (\(s, x) -> exact s $> x) $
    [ ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    ]
