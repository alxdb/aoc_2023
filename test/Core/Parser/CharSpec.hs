module Core.Parser.CharSpec (spec) where

import Prelude

import Control.Applicative (many, some)

import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

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
    describe "lineParser" $ do
      it "parses lines" $
        runParser (lineParser (many anything)) "foo\nbar" `shouldBe` Right (["foo", "bar"], "")
      it "parses lines with trailing newline, without adding an empty result" $
        runParser (lineParser (many anything)) "foo\nbar\n" `shouldBe` Right (["foo", "bar"], "\n")
      it "parses empty lines" $
        runParser (lineParser (many anything)) "foo\n\nbar" `shouldBe` Right (["foo", "", "bar"], "")
      it "parses until first failure" $
        runParser (lineParser (some (exactly 'a'))) "aaa\nbbb\naaa" `shouldBe` Right (["aaa"], "\nbbb\naaa")
