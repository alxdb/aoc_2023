module Aoc23.Solution where

newtype Solution = Solution {runSolution :: String -> Either String Int}
