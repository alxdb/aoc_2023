module Aoc23.Solution where

import Prelude

newtype Solution = Solution {runSolution :: String -> Either String Int}
