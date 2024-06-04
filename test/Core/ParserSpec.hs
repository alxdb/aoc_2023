module Core.ParserSpec (spec) where

import Core.Parser
import Test.Hspec
import Prelude

spec :: Spec
spec =
  describe "Core.ParserSpec" $ do
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
    describe "functor" $ do
      it "applies a function to the parsed value" $
        let p = (== 'h') <$> satisfy (== 'h')
         in runParser p "hello" `shouldBe` Right (True, "ello")
    describe "applicative" $ do
      it "sequences parsers and applies a function to results" $
        let p =
              (\h e l -> h == 'h' && e == 'e' && l == 'l')
                <$> satisfy (== 'h')
                <*> satisfy (== 'e')
                <*> satisfy (== 'l')
         in runParser p "hello" `shouldBe` Right (True, "lo")
    describe "monad" $ do
      it "sequences results of parsers" $
        let p = do
              h <- satisfy (== 'h')
              e <- satisfy (== 'e')
              l1 <- satisfy (== 'l')
              l2 <- satisfy (== 'l')
              o <- satisfy (== 'o')
              return [h, e, l1, l2, o]
         in runParser p "hello" `shouldBe` Right ("hello", "")
    describe "many" $ do
      it "parses multiple instances" $
        let p = many (satisfy (== 'f'))
         in runParser p "fffoobar" `shouldBe` Right ("fff", "oobar")
