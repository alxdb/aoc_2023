module Aoc23.Day01Spec (spec) where

import Prelude

import Aoc23.Day01
import Aoc23.Solution

import Test.Hspec

example_1 :: String
example_1 =
  "1abc2\n\
  \pqr3stu8vwx\n\
  \a1b2c3d4e5f\n\
  \treb7uchet\n"

example_2 :: String
example_2 =
  "two1nine\n\
  \eightwothree\n\
  \abcone2threexyz\n\
  \xtwone3four\n\
  \4nineeightseven2\n\
  \zoneight234\n\
  \7pqrstsixteen\n"

spec :: Spec
spec =
  describe "Aoc23.Day01" $ do
    describe "solution" $ do
      it "solves the first sample solution" $
        runSolution solution example_1 `shouldBe` Right 142
      it "solves the second sample solution" $
        runSolution solution example_2 `shouldBe` Right 281
    describe "shared letters" $
      it "sucessfully parses digits that share letters" $
        runSolution solution "oneight" `shouldBe` Right 18
