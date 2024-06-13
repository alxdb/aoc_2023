module Core.Parser.CharSpec (spec) where

import Prelude

import Core.Parser
import Core.Parser.Char

import Test.Hspec

spec :: Spec
spec =
  describe "Core.Parser.Char" $ do
    describe "digitParser" $ do
      it "parses a digit" $
        runParser digitParser "12" `shouldBe` Right (1, "2")
    describe "intParser" $ do
      it "parses an int" $
        runParser intParser "12" `shouldBe` Right (12, "")
