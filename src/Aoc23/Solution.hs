module Aoc23.Solution where

import Flow
import Prelude

import Data.Bifunctor (Bifunctor (bimap))

import Core.Parser
import Core.Parser.Char

newtype Solution = Solution {runSolution :: String -> Either String Int}

sumLines :: (String -> Either String Int) -> Solution
sumLines lineSolution = Solution $ (sum <$>) . mapM lineSolution . filter (not . null) . lines

sumLinesParser :: ParserC a -> (a -> Int) -> Solution
sumLinesParser p f = sumLines $ parse p .> bimap show f

parserSolution :: ParserC a -> (a -> Int) -> Solution
parserSolution p f = Solution $ parse p .> bimap show f
