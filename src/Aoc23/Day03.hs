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
    [ getElem j i s
    | j <- [(r - 1) .. (r + 1)]
    , i <- [(c - 1) .. (c + 1)]
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
    (Di <$> intParser)
      <|> (Em <$> exactly '.')
      <|> (Gr <$> exactly '*')
      <|> (Sy <$> anything)

  replicateDigit (Di x) = replicate (length . show $ x) (Di x)
  replicateDigit x = [x]

getPartNumbers :: Schematic -> [Int]
getPartNumbers s@(Schematic rows) = V.ifoldl go [] rows
 where
  go results r row =
    let
      getIndexedDigit (i, p) = do
        d <- getDigit p
        return (i, d)
      isPartNum (c, _) = any isSymbol $ neighbours r c s
      results' =
        mapMaybe
          (fmap snd . find isPartNum)
          . adjacentIndexes
          . mapMaybe getIndexedDigit
          $ (V.toList . V.indexed $ row)
     in
      results ++ results'

adjacentIndexes :: [(Int, a)] -> [[(Int, a)]]
adjacentIndexes [] = []
adjacentIndexes (x : xs) = snd $ foldl go (x, [[x]]) xs
 where
  go ((prevI, _), results@(current : others)) next@(nextI, _) =
    if nextI - prevI == 1
      then (next, (next : current) : others)
      else (next, [next] : results)
  go ((_, _), []) (_, _) = undefined
