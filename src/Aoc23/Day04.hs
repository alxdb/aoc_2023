module Aoc23.Day04 (solution_1, solution_2) where

import Aoc23.Solution
import Control.Monad.Extra
import Control.Monad.State
import Core.Parser.Char
import Core.Parser.Combinator
import Data.IntMap hiding (foldl)
import Data.List hiding (insert)
import Data.Vector qualified as V
import Flow
import Prelude

solution_1 :: Solution
solution_1 = sumLinesParser cardParser <| getPoints <. matches
 where
  getPoints x = if x < 1 then 0 else 2 ^ (x - 1)

solution_2 :: Solution
solution_2 = parserSolution (lineParser cardParser) <| totalCardsWon <. V.fromList
 where
  totalCardsWon cards =
    let allWonCards = evalState (concatMapM getWonCards (V.toList cards)) empty
        getWonCards card = gets (!? card.cardId) >>= maybe findCardWins return
         where
          findCardWins = do
            let thisCardWins = [cards V.! (card.cardId + i - 1) | i <- [1 .. matches card]]
            subCardwins <- (++ thisCardWins) <$> concatMapM getWonCards thisCardWins
            state (\cache -> (subCardwins, insert card.cardId subCardwins cache))
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
