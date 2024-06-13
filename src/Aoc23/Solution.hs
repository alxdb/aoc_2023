module Aoc23.Solution where

import Prelude

newtype Solution = Solution {runSolution :: String -> Either String Int}

sumLines :: (String -> Either String Int) -> Solution
sumLines lineSolution = Solution $ (sum <$>) . mapM lineSolution . filter (not . null) . lines
