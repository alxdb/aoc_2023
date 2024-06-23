module Aoc23.Day03Spec (spec) where

import Prelude

import Aoc23.Day03
import Aoc23.Solution
import Core.Parser

import Test.Hspec

sampleInput :: String
sampleInput =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598..\n"

expectedSchematic :: Schematic
expectedSchematic =
  Schematic
    [ [Di 467, Di 467, Di 467, Em '.', Em '.', Di 114, Di 114, Di 114, Em '.', Em '.']
    , [Em '.', Em '.', Em '.', Sy '*', Em '.', Em '.', Em '.', Em '.', Em '.', Em '.']
    , [Em '.', Em '.', Di 035, Di 035, Em '.', Em '.', Di 633, Di 633, Di 633, Em '.']
    , [Em '.', Em '.', Em '.', Em '.', Em '.', Em '.', Sy '#', Em '.', Em '.', Em '.']
    , [Di 617, Di 617, Di 617, Sy '*', Em '.', Em '.', Em '.', Em '.', Em '.', Em '.']
    , [Em '.', Em '.', Em '.', Em '.', Em '.', Sy '+', Em '.', Di 058, Di 058, Em '.']
    , [Em '.', Em '.', Di 592, Di 592, Di 592, Em '.', Em '.', Em '.', Em '.', Em '.']
    , [Em '.', Em '.', Em '.', Em '.', Em '.', Em '.', Di 755, Di 755, Di 755, Em '.']
    , [Em '.', Em '.', Em '.', Sy '$', Em '.', Sy '*', Em '.', Em '.', Em '.', Em '.']
    , [Em '.', Di 664, Di 664, Di 664, Em '.', Di 598, Di 598, Di 598, Em '.', Em '.']
    ]

spec :: Spec
spec = describe "Aoc23.Day03" $ do
  describe "schematicParser" $ do
    it "parses the example schematic" $
      parse schematicParser sampleInput
        `shouldBe` Right expectedSchematic
  describe "getPartNumbers" $ do
    it "extracts part numbers" $
      getPartNumbers expectedSchematic `shouldBe` [598, 664, 755, 592, 617, 633, 35, 467]
  describe "solution_1" $ do
    it "solves the sample solution" $
      runSolution solution_1 sampleInput `shouldBe` Right 4361
