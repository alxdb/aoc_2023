module Test.Day02Spec where

import Prelude

import Test.Spec (Spec, it, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Day02" do
  it "handles the default example" do
    let result = "foo"
    let expect = "foo"
    result `shouldEqual` expect
