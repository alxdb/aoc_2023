module Test.Aoc23.Day03Spec where

import Prelude

import Aoc23.Day03 (day03)

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Day03" do
  it "handles the default example" do
    let
      example =
        """467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"""
    let result = day03 example
    let expect = pure 4361
    result `shouldEqual` expect
