module Aoc23.Day02 where

import Prelude hiding (id)

import Aoc23.Solution
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

solution_1 :: Solution
solution_1 = sumLines lineSolution_1

lineSolution_1 :: String -> Either String Int
lineSolution_1 _ = Left "incomplete"

data Game = Game {id :: Int, hands :: [Hand]}
data Hand = Hand {red :: Int, green :: Int, blue :: Int}
data Cube = Red | Green | Blue

gameParser :: Parser Char Game
gameParser = do
  exact "Game "
  id <- intParser
  exact ": "
  hands <- sepBySome handParser (exact "; ")
  return $ Game{id, hands}

handParser :: Parser Char Hand
handParser = do
  cubes <- sepBySome cubeParser (exact ", ")
  return $ asHand cubes

cubeParser :: Parser Char (Int, Cube)
cubeParser = do
  count <- intParser
  exactly ' '
  cube <- exactMapping [("red", Red), ("green", Green), ("blue", Blue)]
  return (count, cube)

asHand :: [(Int, Cube)] -> Hand
asHand = undefined

addCube :: (Int, Cube) -> Hand -> Hand

-- addCube (x, Red) Hand{red, green, blue} = Hand{.red = red + x, green, blue}
