module Aoc23.Day04 (solution_1, solution_2) where

import Flow
import Prelude

import Data.List

import Data.IntMap qualified as IM

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
  go line = do
    card <- parseShow cardParser line
    let points =
          card
            |> matches
            |> (\x -> if x < 1 then 0 else 2 ^ (x - 1))
    return points

solution_2 :: Solution
solution_2 = Solution go
 where
  go input = do
    cards <- parseShow (lineParser cardParser) input
    let cardsById =
          cards
            |> map (\card@Card{cardId} -> (cardId, card))
            |> IM.fromList
    let allWonCards = concatMap (winningCards cardsById) cards
    return $ length allWonCards + length cards

matches :: Card -> Int
matches Card{wins, have} = length $ intersect wins have

winningCards :: IM.IntMap Card -> Card -> [Card]
winningCards cards card@Card{cardId} =
  let
    thisCardWins = [cards IM.! (cardId + i) | i <- [1 .. matches card]]
   in
    thisCardWins ++ concatMap (winningCards cards) thisCardWins
