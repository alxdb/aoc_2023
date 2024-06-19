module Core.Parser.CharSpec (spec) where

import Control.Applicative (many)
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
    describe "lineParser" $ do
      it "parses lines" $
        runParser (lineParser (many (satisfy (/= '\n')))) "foo\nbar" `shouldBe` Right (["foo", "bar"], "")
      it "parses lines with trailing newline" $
        runParser (lineParser (many (satisfy (/= '\n')))) "foo\nbar\n" `shouldBe` Right (["foo", "bar"], "")
      it "parses empty lines" $
        runParser (lineParser (many (satisfy (/= '\n')))) "foo\n\nbar" `shouldBe` Right (["foo", "", "bar"], "")
