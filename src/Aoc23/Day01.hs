module Aoc23.Day01 (solution) where

import Aoc23.Solution
import Control.Applicative
import Core.Parser
import Data.Char
import Data.EitherR

solution :: Solution
solution = Solution $ (sum <$>) . mapM calibrationValue . filter (not . null) . lines

calibrationValue :: String -> Either String Int
calibrationValue = fmapL errorMessage . parse p
  where
    p = do
      ds <- some nextDigit
      return $ head ds * 10 + last ds

nextDigit :: Parser Char Int
nextDigit = p
  where
    p = do
      _ <- many (satisfy (not . isDigit))
      d <- satisfy isDigit
      return $ digitToInt d

errorMessage :: [Error Char] -> String
errorMessage [Empty] = "Couldn't find a digit in input line"
errorMessage x = show x
