module Main where

import Prelude

import Day01 (day01)
import Day02 (day02)
import Day03 (day03)

import Data.Maybe (Maybe(..))
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Effect (Effect)
import Effect.Console (logShow, log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Options.Applicative as Opts

type Args = { day :: Int, inputFile :: String }

argParser :: Opts.Parser Args
argParser = ado
  day <- Opts.argument Opts.int (Opts.metavar "DAY")
  inputFile <- Opts.argument Opts.str (Opts.metavar "INPUT_FILE")
  in { day, inputFile }

argParserInfo :: Opts.ParserInfo Args
argParserInfo = Opts.info (argParser Opts.<**> Opts.helper) Opts.briefDesc

pickDay :: Int -> Effect (Maybe (String -> Maybe Int))
pickDay n =
  let
    just = pure <<< pure
  in
    case n of
      1 -> just day01
      2 -> just day02
      3 -> just day03
      _ -> do
        log "Day not done!"
        pure Nothing

runDay :: (String -> Maybe Int) -> String -> Effect (Maybe Int)
runDay dayFn inputFile = do
  input <- readTextFile UTF8 inputFile
  case dayFn input of
    Nothing -> do
      log "Day failed!"
      pure Nothing
    val -> pure val

main :: Effect Unit
main = do
  { day, inputFile } <- Opts.execParser argParserInfo
  result <- runMaybeT do
    dayFn <- MaybeT $ pickDay day
    MaybeT $ runDay dayFn inputFile
  logShow result
