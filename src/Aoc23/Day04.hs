module Aoc23.Day04 (solution_1) where

import Data.List
import Prelude hiding (id)

import Control.Error (fmapL)
import Flow

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

solution_1 :: Solution
solution_1 = sumLines lineSolution

lineSolution :: String -> Either String Int
lineSolution line = do
  card <- parse cardParser line |> fmapL show
  card
    |> matches
    |> (\x -> if x < 1 then 0 else 2 ^ (x - 1))
    |> return

data Card = Card
  { id :: Int
  , wins :: [Int]
  , have :: [Int]
  }
  deriving (Show, Eq)

cardParser :: ParserC Card
cardParser = do
  id <- exact "Card" >> spaces *> intParser <* (exactly ':' >> spaces)
  wins <- endBySome intParser spaces (exactly '|' |> inSpaces)
  have <- sepBySome intParser spaces
  return $ Card{id, wins, have}

matches :: Card -> Int
matches Card{wins, have} = wins `intersect` have |> length
