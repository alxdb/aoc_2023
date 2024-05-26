module Test.Day01Spec where

import Prelude

import Day01 (extractCallibrationValues)

import Data.Maybe (Maybe(..))
import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Day01" do
  it "handles the default example" do
    let example = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet\n"
    let result = extractCallibrationValues example
    let expect = Just [ 12, 38, 15, 77 ]
    result `shouldEqual` expect

