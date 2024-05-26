module Test.Day01Spec where

import Prelude

import Day01 (extractCallibrationValues)

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
    let result = extractCallibrationValues example
    let expect = Just [ 12, 38, 15, 77 ]
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
    let result = extractCallibrationValues example
    let expect = Just [ 29, 83, 13, 24, 42, 14, 76 ]
    result `shouldEqual` expect

