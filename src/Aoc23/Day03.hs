module Aoc23.Day03 (solution_1, schematicParser, Schematic (..), Part (..), getPartNumbers) where

import Prelude

import Control.Applicative
import Data.List
import Data.Maybe

import Data.Vector (Vector, (!?))
import Data.Vector qualified as V

import Aoc23.Solution
import Control.Error (fmapL)
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator hiding (next)

solution_1 :: Solution
solution_1 = Solution $ \input -> do
  schematic <- fmapL show $ parse schematicParser input
  let partNumbers = getPartNumbers schematic
  return . sum $ partNumbers

type Grid a = Vector (Vector a)

newtype Schematic where
  Schematic :: Grid Part -> Schematic
  deriving (Show, Eq)

data Part where
  Di :: Int -> Part
  Em :: Char -> Part
  Gr :: Char -> Part
  Sy :: Char -> Part
  deriving (Show, Eq)

getDigit :: Part -> Maybe Int
getDigit (Di i) = Just i
getDigit _ = Nothing

isSymbol :: Part -> Bool
isSymbol (Sy _) = True
isSymbol (Gr _) = True
isSymbol _ = False

getElem :: Int -> Int -> Schematic -> Maybe Part
getElem r c (Schematic s) = (s !? r) >>= (!? c)

neighbours :: Int -> Int -> Schematic -> [Part]
neighbours r c s =
  catMaybes
    [ getElem r' c' s
    | r' <- [(r - 1) .. (r + 1)]
    , c' <- [(c - 1) .. (c + 1)]
    ]

schematicParser :: ParserC Schematic
schematicParser =
  Schematic
    . V.fromList
    . map V.fromList
    <$> lineParser schematicLineParser
 where
  schematicLineParser = concatMap replicateDigit <$> many partParser
  partParser =
    (Em <$> exactly '.')
      <|> (Di <$> intParser)
      <|> (Gr <$> exactly '*')
      <|> (Sy <$> anything)

  replicateDigit (Di x) = replicate (length . show $ x) (Di x)
  replicateDigit x = [x]

getPartNumbers :: Schematic -> [Int]
getPartNumbers s@(Schematic rows) = V.ifoldl go [] rows
 where
  go results r row =
    let
      results' =
        mapMaybe
          (fmap snd . find isPartNum)
          . contiguousElements
          . mapMaybe (\(c, p) -> (c,) <$> getDigit p)
          $ (V.toList . V.indexed $ row)
     in
      results ++ results'
   where
    isPartNum (c, _) = any isSymbol $ neighbours r c s

contiguousElements :: [(Int, a)] -> [[(Int, a)]]
contiguousElements [] = []
contiguousElements (x : xs) = foldl' go [[x]] xs
 where
  go (current : rest) y@(i, _)
    | i == succ (fst (head current)) = (y : current) : rest
    | otherwise = [y] : current : rest
  go _ _ = undefined
