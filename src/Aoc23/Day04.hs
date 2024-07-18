module Aoc23.Day04 (solution_1, solution_2) where

import Aoc23.Solution
import Core.Parser.Char
import Core.Parser.Combinator
import Data.List
import Data.Vector (Vector)
import Data.Vector qualified as V
import Flow
import Prelude

solution_1 :: Solution
solution_1 = sumLinesParser cardParser <| getPoints <. matches
 where
  getPoints x = if x < 1 then 0 else 2 ^ (x - 1)

solution_2 :: Solution
solution_2 = parserSolution (lineParser cardParser) <| go <. V.fromList
 where
  go cards =
    let allWonCards = concatMap (winningCards cards) cards
     in length allWonCards + length cards

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
  return $ Card {cardId, wins, have}

matches :: Card -> Int
matches Card {wins, have} = length $ intersect wins have

winningCards :: Vector Card -> Card -> [Card]
winningCards cards card@Card {cardId} =
  let thisCardWins = [cards V.! (cardId + i - 1) | i <- [1 .. matches card]]
   in thisCardWins ++ concatMap (winningCards cards) thisCardWins
