module Aoc23.Day01 (solution) where

import Control.Applicative
import Core.Parser
import Data.Char

solution :: String -> ParserResult Char Int
solution = (sum <$>) . mapM parseDigits . filter (not . null) . lines

parseDigits :: String -> ParserResult Char Int
parseDigits = parse p
  where
    p = do
      ds <- some parseDigit
      return $ head ds * 10 + last ds

parseDigit :: Parser Char Int
parseDigit = p
  where
    p = do
      _ <- many (satisfy isAlpha)
      d <- satisfy isDigit
      return $ digitToInt d
