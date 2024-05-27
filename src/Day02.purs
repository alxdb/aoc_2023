module Day02 (day02) where

import Prelude

import Debug (spy)

import Control.Alt ((<|>))
import Data.Either (hush)
import Data.Foldable (sum, foldl, find)
import Data.List (List)
import Data.Maybe (Maybe, isNothing)
import Data.String.Utils (lines, trimEnd)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Parsing (Parser, runParser)
import Parsing.Combinators (sepBy)
import Parsing.String (string)
import Parsing.String.Basic (intDecimal, space)

type Hand = { r :: Int, g :: Int, b :: Int }
type Game = { id :: Int, hands :: List Hand }
data Cube = R | G | B

day02 :: String -> Maybe Int
day02 input = sum <$> (trimEnd >>> lines >>> traverse go) input
  where
  go line = do
    game <- parseGame (spy "line" line)
    pure $ if (spy "isPossible" $ isGamePossible game) then game.id else 0

isGamePossible :: Game -> Boolean
isGamePossible game = isNothing $ find isHandImpossible (spy "hands" game.hands)
  where
  isHandImpossible { r, g, b } = (r > 12) || (g > 13) || (b > 14)

parseGame :: String -> Maybe Game
parseGame line = hush $ runParser line gameParser

gameParser :: Parser String Game
gameParser = do
  id <- string "Game " *> intDecimal
  hands <- string ": " *> handParser `sepBy` string "; "
  pure { id: id, hands: hands }

handParser :: Parser String Hand
handParser = do
  cubes <- cubeParser `sepBy` string ", "
  pure $ foldl f { r: 0, g: 0, b: 0 } cubes
  where
  f counts (Tuple cube count) = case cube of
    R -> counts { r = count }
    G -> counts { g = count }
    B -> counts { b = count }

cubeParser :: Parser String (Tuple Cube Int)
cubeParser = do
  count <- intDecimal
  cube <- space *>
    ( string "red" *> pure R
        <|> string "green" *> pure G
        <|> string "blue" *> pure B
    )
  pure $ Tuple cube count

