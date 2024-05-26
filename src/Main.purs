module Main where

import Prelude

import Day01 (day01)
import Day02 (day02)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Options.Applicative as Opts
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Args = { day :: Int, inputFile :: String }

argParser :: Opts.Parser Args
argParser = ado
  day <- Opts.argument Opts.int (Opts.metavar "DAY")
  inputFile <- Opts.argument Opts.str (Opts.metavar "INPUT_FILE")
  in { day, inputFile }

argParserInfo :: Opts.ParserInfo Args
argParserInfo = Opts.info (argParser Opts.<**> Opts.helper) Opts.briefDesc

pickDay :: Int -> Maybe (String -> Maybe Int)
pickDay n = case n of
  1 -> Just day01
  2 -> Just day02
  _ -> Nothing

main :: Effect Unit
main = go =<< Opts.execParser argParserInfo
  where
  go { day, inputFile } = case pickDay day of
    Nothing -> log "Day not done!"
    Just dayFn -> do
      input <- readTextFile UTF8 inputFile
      case dayFn input of
        Nothing -> log "Day failed!"
        Just result -> log $ show result
