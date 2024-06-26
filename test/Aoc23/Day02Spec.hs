module Aoc23.Day02Spec (spec) where

import Prelude

import Aoc23.Day02
import Aoc23.Solution

import Test.Hspec

exampleGames :: String
exampleGames =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
  \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
  \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
  \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
  \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

spec :: Spec
spec = describe "Aoc23.Day02" $ do
  describe "solution" $ do
    it "solves the first problem" $
      runSolution solution_1 exampleGames
        `shouldBe` Right 8
    it "solves the second problem" $
      runSolution solution_2 exampleGames
        `shouldBe` Right 2286
