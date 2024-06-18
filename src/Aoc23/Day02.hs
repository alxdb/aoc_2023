module Aoc23.Day02 (solution_1) where

import Prelude hiding (id)

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator
import Data.Bifunctor (Bifunctor (bimap))

solution_1 :: Solution
solution_1 = sumLines lineSolution_1

solution_1_hand :: Hand
solution_1_hand = Hand{red = 12, green = 13, blue = 14}

lineSolution_1 :: String -> Either String Int
lineSolution_1 = bimap show go . parse gameParser
 where
  go game = if gameIsPossible solution_1_hand game then game.id else 0

data Game = Game {id :: Int, hands :: [Hand]}
data Hand = Hand {red :: Int, green :: Int, blue :: Int}
data Cube = Red | Green | Blue

gameIsPossible :: Hand -> Game -> Bool
gameIsPossible refHand Game{hands} = all handIsPossible hands
 where
  handIsPossible Hand{red, green, blue} = red <= refHand.red && green <= refHand.green && blue <= refHand.blue

gameParser :: Parser Char Game
gameParser = do
  _ <- exact "Game "
  id <- intParser
  _ <- exact ": "
  hands <- sepBySome handParser (exact "; ")
  return $ Game{id, hands}

handParser :: Parser Char Hand
handParser = do
  cubes <- sepBySome cubeParser (exact ", ")
  return $ asHand cubes

cubeParser :: Parser Char (Int, Cube)
cubeParser = do
  count <- intParser
  _ <- exactly ' '
  cube <- exactMapping [("red", Red), ("green", Green), ("blue", Blue)]
  return (count, cube)

asHand :: [(Int, Cube)] -> Hand
asHand = foldl addCube (Hand 0 0 0)

addCube :: Hand -> (Int, Cube) -> Hand
addCube hand (x, Red) = hand{red = hand.red + x}
addCube hand (x, Green) = hand{green = hand.green + x}
addCube hand (x, Blue) = hand{blue = hand.blue + x}
