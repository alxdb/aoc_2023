module Aoc23.Day04 (solution) where

import Flow
import Prelude hiding (id)

import Data.List

import Control.Error (fmapL)

import Aoc23.Solution

import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

solution :: Solution
solution = sumLines lineSolution

lineSolution :: String -> Either String Int
lineSolution line = do
  card <- fmapL show <| parse cardParser line
  let points = matches card |> (\x -> if x < 1 then 0 else 2 ^ (x - 1))
  return points

data Card = Card
  { id :: Int
  , wins :: [Int]
  , have :: [Int]
  }
  deriving (Show, Eq)

cardParser :: ParserC Card
cardParser = do
  id <- exact "Card " *> intParser <* (exactly ':' >> spaces)
  wins <- endBySome intParser spaces (exactly '|' |> inSpaces)
  have <- sepBySome intParser spaces
  return $ Card{id, wins, have}

matches :: Card -> Int
matches Card{wins, have} = wins `intersect` have |> length
