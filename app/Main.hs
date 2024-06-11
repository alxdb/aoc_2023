module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dayN] -> print dayN
    _ -> fail "Provide day number as first argument"
