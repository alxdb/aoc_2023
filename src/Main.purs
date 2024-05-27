module Main where

import Prelude

import Aoc23.Day01 (day01)
import Aoc23.Day02 (day02)
import Aoc23.Day03 (day03)

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Plus (empty)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (try)
import Effect.Console (logShow, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Process (setExitCode)
import Options.Applicative as Opts

type Args = { dayNum :: Int, inputFile :: String }
type DayFn = String -> Maybe Int

argParser :: Opts.Parser Args
argParser = ado
  dayNum <- Opts.argument Opts.int (Opts.metavar "DAY_NUM")
  inputFile <- Opts.argument Opts.str (Opts.metavar "INPUT_FILE")
  in { dayNum, inputFile }

argParserInfo :: Opts.ParserInfo Args
argParserInfo = Opts.info (argParser Opts.<**> Opts.helper) Opts.briefDesc

main :: Effect Unit
main = do
  { dayNum, inputFile } <- Opts.execParser argParserInfo
  success <- runMaybeT do
    dayFn <- case dayNum of
      1 -> pure day01
      2 -> pure day02
      3 -> pure day03
      _ -> (lift $ log "Day not done!") *> empty
    input <- do
      inputE <- lift $ try (readTextFile UTF8 inputFile)
      case inputE of
        Right input -> pure input
        Left _ -> (lift $ log "Cannot open input file! ") *> empty
    case dayFn input of
      Just x -> lift $ logShow x
      _ -> (lift $ log "Day failed!") *> empty
  case success of
    Just _ -> pure unit
    _ -> setExitCode 1
