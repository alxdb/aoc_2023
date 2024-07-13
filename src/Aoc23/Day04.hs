module Aoc23.Day04 (solution_1, solution_2) where

import Flow
import Prelude

import Data.List

import Data.Vector (Vector)
import Data.Vector qualified as V

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

data Card = Card
  { cardId :: Int
  , wins :: [Int]
  , have :: [Int]
  }
  deriving (Show, Eq)

cardParser :: ParserC Card
cardParser = do
  cardId <- exact "Card" >> spaces *> intParser <* (exactly ':' >> spaces)
  wins <- endBySome intParser spaces (exactly '|' |> inSpaces)
  have <- sepBySome intParser spaces
  return $ Card{cardId, wins, have}

solution_1 :: Solution
solution_1 = sumLines go
 where
  getPoints x = if x < 1 then 0 else 2 ^ (x - 1)
  go line = do
    card <- parseShow cardParser line
    let points = card |> matches |> getPoints
    return points

matches :: Card -> Int
matches Card{wins, have} = length $ intersect wins have

solution_2 :: Solution
solution_2 = Solution go
 where
  go input = do
    cards <- V.fromList <$> parseShow (lineParser cardParser) input
    let allWonCards = concatMap (winningCards cards) cards
    return $ length allWonCards + length cards

winningCards :: Vector Card -> Card -> [Card]
winningCards cards card@Card{cardId} =
  let
    thisCardWins = [cards V.! (cardId + i - 1) | i <- [1 .. matches card]]
   in
    thisCardWins ++ concatMap (winningCards cards) thisCardWins
