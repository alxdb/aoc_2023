module Test.Aoc23.Day01Spec where

import Prelude

import Aoc23.Day01 (day01)

import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Day01" do
  it "handles the first example" do
    let
      example =
        """1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
"""
    let result = day01 example
    let expect = pure $ sum [ 12, 38, 15, 77 ]
    result `shouldEqual` expect
  it "handles the second example" do
    let
      example =
        """two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
"""
    let result = day01 example
    let expect = pure $ sum [ 29, 83, 13, 24, 42, 14, 76 ]
    result `shouldEqual` expect
  it "returns Nothing when input is invalid" do
    let example = "1abc2\nfoobar\nreb7uchet\n"
    let result = day01 example
    let expect = Nothing
    result `shouldEqual` expect

