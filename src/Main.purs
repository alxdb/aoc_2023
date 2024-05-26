module Main where

import Prelude

import Day01 (day01)
import Effect (Effect)
import Effect.Console (log)
import Options.Applicative as Opts

type Args = { day :: Int, input :: String }

argParser :: Opts.Parser Args
argParser = ado
  day <- Opts.argument Opts.int (Opts.metavar "DAY")
  input <- Opts.argument Opts.str (Opts.metavar "INPUT")
  in { day, input }

main :: Effect Unit
main = runDay =<< Opts.execParser (Opts.info (argParser Opts.<**> Opts.helper) Opts.briefDesc)
  where
  runDay { day: 1, input: input } = day01 input
  runDay _ = log "Not solved yet!"
