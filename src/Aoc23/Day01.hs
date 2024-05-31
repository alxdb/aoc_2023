module Aoc23.Day01 (solution) where

import Data.Char (digitToInt)
import Data.Text (Text)
import Text.Parsec qualified as PC
import Text.Parsec.Text (Parser)
import Prelude (Either, Int, fail, fmap, return, sum, undefined, (*), (+), (.), (<$>), (>>=))

solution :: Text -> Either PC.ParseError Int
solution = (sum . fmap toVal <$>) . parseInput
  where
    toVal :: (Int, Int) -> Int
    toVal (fst, lst) = (fst * 10) + lst

parseInput :: Text -> Either PC.ParseError [(Int, Int)]
parseInput = PC.runParser inputParser () "input"

inputParser :: Parser [(Int, Int)]
inputParser = do
  PC.skipMany PC.newline
  result <- digitsParser `PC.sepEndBy` PC.skipMany PC.newline
  PC.eof
  return result
  where
    digitsParser = PC.manyAccum keepFirstAndLast nextDigitParser >>= asTuple
    nextDigitParser =
      digitToInt <$> do
        PC.skipMany PC.letter
        d <- PC.digit
        PC.skipMany PC.letter
        return d

    keepFirstAndLast d [] = [d]
    keepFirstAndLast d2 [d1] = [d1, d2]
    keepFirstAndLast d3 [d1, _] = [d1, d3]
    keepFirstAndLast _ _ = undefined

    asTuple [a, b] = return (a, b)
    asTuple [a] = return (a, a)
    asTuple _ = fail "expected exactly one or two numbers"
