module Aoc23.Day03 (solution, schematicParser, Schematic(..), Part(..), getPartNumbers) where

import Control.Applicative
import Control.Monad.ST
import Data.STRef
import Prelude

import Data.Vector (Vector, fromList, ifoldM, ifoldl, (!?))

import Aoc23.Solution
import Control.Error (fmapL)
import Core.Parser
import Core.Parser.Char
import Core.Parser.Combinator

solution :: Solution
solution = Solution $ \input -> do
  schematic <- fmapL show $ parse schematicParser input
  let partNumbers = getPartNumbers schematic
  return . sum $ partNumbers

newtype Schematic where
  Schematic :: Vector (Vector Part) -> Schematic
  deriving (Show, Eq)

data Part where
  Di :: Int -> Part
  Sy :: Char -> Part
  Em :: Char -> Part
  deriving (Show, Eq)

isSy :: Part -> Bool
isSy (Sy _) = True
isSy _ = False

schematicParser :: ParserC Schematic
schematicParser = Schematic . fromList . map fromList <$> lineParser schematicLine
 where
  schematicLine :: ParserC [Part]
  schematicLine = concatMap replicateDigit <$> many partParser

  partParser :: ParserC Part
  partParser = Em <$> exactly '.' <|> Di <$> intParser <|> Sy <$> satisfy (/= '\n')

  replicateDigit :: Part -> [Part]
  replicateDigit (Di x) = replicate (length . show $ x) (Di x)
  replicateDigit x = [x]

getPartNumbers :: Schematic -> [Int]
getPartNumbers (Schematic s) = ifoldl perLine [] s
 where
  perLine :: [Int] -> Int -> Vector Part -> [Int]
  perLine results row line = runST $ do
    ref <- newSTRef False
    let go lineResults col part = do
          let indexSch i j = (s !? j) >>= (!? i)
          let isPartNo =
                or
                  [ maybe False isSy $ indexSch i j
                  | j <- [(row - 1) .. (row + 1)]
                  , i <- [(col - 1) .. (col + 1)]
                  ]
          case part of
            (Di x) -> do
              inDigit <- readSTRef ref
              if inDigit
                then return lineResults
                else
                  if isPartNo
                    then do
                      writeSTRef ref True
                      return (x : lineResults)
                    else return lineResults
            _ -> do
              writeSTRef ref False
              return lineResults
    ifoldM go results line
