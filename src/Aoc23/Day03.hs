module Aoc23.Day03 (
  solution_1,
  solution_2,
  schematicParser,
  Schematic (..),
  Part (..),
  getPartNumbers,
  getGearRatios,
) where

import Flow
import Prelude

import Control.Applicative
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
  return $ sum partNumbers

solution_2 :: Solution
solution_2 = Solution $ \input -> do
  schematic <- fmapL show $ parse schematicParser input
  let gearRatios = getGearRatios schematic
  return $ sum gearRatios

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

isGear :: Part -> Bool
isGear (Gr _) = True
isGear _ = False

getElem :: Int -> Int -> Schematic -> Maybe Part
getElem r c (Schematic s) = (s !? r) >>= (!? c)

neighbours :: Schematic -> (Int, Int) -> [((Int, Int), Part)]
neighbours s (r, c) =
  catMaybes
    [ ((r', c'),) <$> getElem r' c' s
    | r' <- [(r - 1) .. (r + 1)]
    , c' <- [(c - 1) .. (c + 1)]
    ]

schematicParser :: ParserC Schematic
schematicParser =
  lineParser schematicLineParser
    |> fmap V.fromList
    |> fmap Schematic
 where
  schematicLineParser =
    many partParser
      |> fmap (concatMap repeatIfDigit)
      |> fmap V.fromList
  partParser =
    (Em <$> exactly '.')
      <|> (Di <$> intParser)
      <|> (Gr <$> exactly '*')
      <|> (Sy <$> anything)

  repeatIfDigit d@(Di x) =
    let
      l = show x |> length
     in
      replicate l d
  repeatIfDigit x = [x]

indexedSchematic :: Schematic -> [((Int, Int), Part)]
indexedSchematic (Schematic rows) =
  rows
    |> indexedList
    |> concatMap (second indexedList .> flattenIndexes)
 where
  indexedList = V.indexed .> V.toList
  flattenIndexes (r, indexedRow) = map (flattenIndex r) indexedRow
  flattenIndex r (c, part) = ((r, c), part)

-- group elements if they are successive in a row
groupBySucc :: [((Int, Int), a)] -> [[((Int, Int), a)]]
groupBySucc [] = []
groupBySucc (x : xs) = foldl go [[x]] xs
 where
  go (x' : xs') v@((r, c), _)
    | let
        -- column of previous element
        ((_, c'), _) = head x'
       in
        -- row is equal
        -- and column of current element is one more than previous element
        (r, c) == (r, succ c') =
        -- append current element to current grouping
        (v : x') : xs'
    -- create new group
    | otherwise = [v] : x' : xs'
  -- will never happen, accumulator is always populated
  go [] _ = undefined

getPartNumbers :: Schematic -> [Int]
getPartNumbers s =
  indexedSchematic s
    |> mapMaybe (secondM getDigit)
    |> groupBySucc
    |> mapMaybe getFirstPartNum
 where
  getFirstPartNum = mapMaybe getPartNum .> listToMaybe

  getPartNum (i, d)
    | any (snd .> isSymbol) $ neighbours s i = Just d
    | otherwise = Nothing

getGearRatios :: Schematic -> [Int]
getGearRatios s =
  indexedSchematic s
    |> filter (snd .> isGear)
    |> map getNeighbouringPartNumbers
    |> mapMaybe getGearRatio
 where
  getNeighbouringPartNumbers =
    fst
      .> neighbours s
      .> mapMaybe (secondM getDigit)
      .> groupBySucc
      .> map (snd . head)

  getGearRatio :: [Int] -> Maybe Int
  getGearRatio [a, b] = Just $ a * b
  getGearRatio _ = Nothing
