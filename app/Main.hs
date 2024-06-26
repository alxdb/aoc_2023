module Main where

import Prelude

import Control.Error (note)
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readMaybe)

import Aoc23.Day01 qualified as D1
import Aoc23.Day02 qualified as D2
import Aoc23.Day03 qualified as D3
import Aoc23.Solution
import Paths_aoc2023

newtype Args = Args {dayN :: Int}

liftEither :: Either String b -> IO b
liftEither = either fail return

parseArgs :: IO Args
parseArgs = do
  args <- getArgs
  case args of
    [dayN] -> do
      parsed <- liftEither . note "Invalid argument: day number must be integer" . readMaybe $ dayN
      return (Args parsed)
    _ -> fail "Provide day number as first argument"

getSolution :: Args -> IO Solution
getSolution (Args dayN) = case dayN of
  1 -> return D1.solution
  2 -> return D2.solution_2
  3 -> return D3.solution
  _ -> fail "Solution not complete"

getInput :: Args -> IO String
getInput (Args dayN) = do
  fp <- getDataFileName $ printf "input/day%02d.txt" dayN
  readFile fp

main :: IO ()
main = do
  args <- parseArgs
  solution <- getSolution args
  input <- getInput args
  answer <- liftEither . runSolution solution $ input
  print answer
