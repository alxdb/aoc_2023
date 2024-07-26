module Aoc23.Day05 (solution_1) where

import Prelude
import Flow

import Aoc23.Solution

import Core.Parser.Char

solution_1 :: Solution
solution_1 = parserSolution almanacParser (min <. findLocation)

newtype Almanac = Almanac {seeds :: [Int]} deriving (Show, Eq)

almanacParser :: ParserC Almanac
almanacParser = return $ Almanac [1,2,3]

findLocation :: Almanac -> [Int]
findLocation _ = [1,2,3]
