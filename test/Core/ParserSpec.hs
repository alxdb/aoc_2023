module Core.ParserSpec (spec) where

import Prelude

import Control.Applicative

import Core.Parser

import Test.Hspec

spec :: Spec
spec =
  describe "Core.Parser" $ do
    describe "satisfy" $ do
      it "parses tokens matching a predicate" $
        let p = satisfy (== 'h')
         in runParser p "hello" `shouldBe` Right ('h', "ello")
      it "returns error when input is empty" $
        let p = satisfy (== 'h')
         in runParser p "" `shouldBe` Left [Empty]
      it "returns Unexpected when predicate does not match" $
        let p = satisfy (== 'h')
         in runParser p "goodbye" `shouldBe` Left [Unexpected 'g']
    describe "end" $ do
      it "returns unit when input is empty" $
        runParser end "" `shouldBe` Right ((), [])
      it "returns error when input is not empty" $
        runParser end "hello" `shouldBe` Left [Unexpected 'h']
    describe "applicative" $ do
      it "combines parsers sequentially" $
        let p =
              (\h e l -> [h, e, l])
                <$> satisfy (== 'h')
                <*> satisfy (== 'e')
                <*> satisfy (== 'l')
         in runParser p "hello" `shouldBe` Right ("hel", "lo")
      it "combines errors sequentially" $
        let p =
              (\h e l -> h == 'h' && e == 'e' && l == 'l')
                <$> satisfy (== 'h')
                <*> satisfy (== 'e')
                <*> satisfy (== 'l')
         in runParser p "hi" `shouldBe` Left [Unexpected 'i']
    describe "monad" $ do
      it "sequences parsers" $
        let p = do
              h <- satisfy (== 'h')
              e <- satisfy (== 'e')
              l <- satisfy (== 'l')
              return [l, e, h]
         in runParser p "hello" `shouldBe` Right ("leh", "lo")
    describe "alternative" $ do
      it "combines parsers" $
        let p = satisfy (== 'l') <|> satisfy (== 'r')
         in do
              runParser p "l" `shouldBe` Right ('l', "")
              runParser p "r" `shouldBe` Right ('r', "")
      it "combines errors" $
        let p = satisfy (== 'g') <|> (satisfy (== 'h') >> satisfy (== 'i'))
         in runParser p "hello" `shouldBe` Left [Unexpected 'h', Unexpected 'e']
