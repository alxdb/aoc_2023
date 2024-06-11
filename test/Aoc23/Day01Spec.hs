module Aoc23.Day01Spec (spec) where

import Aoc23.Day01
import Aoc23.Solution
import Test.Hspec

example_1 :: String
example_1 =
  "1abc2\n\
  \pqr3stu8vwx\n\
  \a1b2c3d4e5f\n\
  \treb7uchet\n"

spec :: Spec
spec =
  describe "Aoc23.Day01" $
    describe "solution" $
      it "solves the sample solution" $
        runSolution solution example_1 `shouldBe` Right 142
