module Core.Parser.CombinatorSpec (spec) where

import Prelude

import Core.Parser
import Core.Parser.Combinator

import Test.Hspec

spec :: Spec
spec =
  describe "Core.Parser.Combinator" $ do
    describe "anything" $ do
      it "returns anything when there is input" $
        runParser anything "hello" `shouldBe` Right ('h', "ello")
      it "returns error when input is empty" $
        runParser anything "" `shouldBe` Left [Empty]
    describe "exact" $ do
      it "parses an exact sequence" $
        let p = exact "hello"
         in runParser p "hello world" `shouldBe` Right ("hello", " world")
    describe "next" $ do
      it "returns the next successful parse" $
        let p = next $ exact "world"
         in runParser p "hello world again" `shouldBe` Right ("world", " again")
      it "returns empty if no successful parse" $
        let p = next $ exact "hi"
         in runParser p "hello" `shouldBe` Left [Empty]
    describe "lookAhead" $ do
      it "parses without consuming input" $
        let p = lookAhead (exact "hello")
         in runParser p "hello" `shouldBe` Right ("hello", "hello")
