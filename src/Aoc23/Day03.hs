module Aoc23.Day03 (
  solution_1,
  solution_2,
  schematicParser,
  Schematic (..),
  Part (..),
  getPartNumbers,
  getGearRatios,
) where

import Prelude

import Control.Applicative
import Data.List
import Data.Maybe

import Data.Tuple.Extra
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

solution_2 :: Solution
solution_2 = Solution $ \input -> do
  schematic <- fmapL show $ parse schematicParser input
  let gearRatios = getGearRatios schematic
  return . sum $ gearRatios

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

ineighbours :: Int -> Int -> Schematic -> [((Int, Int), Part)]
ineighbours r c s =
  catMaybes
    [ ((r', c'),) <$> getElem r' c' s
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
          . mapMaybe (secondM getDigit)
          $ (V.toList . V.indexed $ row)
     in
      results ++ results'
   where
    isPartNum (c, _) = any isSymbol $ neighbours r c s

getGearRatios :: Schematic -> [Int]
getGearRatios s@(Schematic rows) = V.ifoldl go [] rows
 where
  go results r row = V.ifoldl go' results row
   where
    go' results' c (Gr _) =
      let
        gearRatio =
          calculateGearRatio
            . map (snd . head)
            . contiguousElements
            . map globalIndex
            . mapMaybe (secondM getDigit)
            $ ineighbours r c s
       in
        case gearRatio of
          Just x -> x : results'
          Nothing -> results'
    go' results' _ _ = results'

    globalIndex ((r', c'), d) = ((r' * V.length row) + c', d)

  calculateGearRatio [a, b] = Just $ a * b
  calculateGearRatio _ = Nothing

contiguousElements :: [(Int, a)] -> [[(Int, a)]]
contiguousElements [] = []
contiguousElements (x : xs) = foldl' go [[x]] xs
 where
  go (current : rest) y@(i, _)
    | i == succ (fst (head current)) = (y : current) : rest
    | otherwise = [y] : current : rest
  go _ _ = undefined
