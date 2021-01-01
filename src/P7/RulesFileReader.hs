module P7.RulesFileReader(
  readRulesFile
  ) where

import System.IO

import P7.Rules(
  buildRules,
  Rules
  )

readRulesFile :: String -> IO Rules
readRulesFile fileName = do
  readLines <- lines <$> readFile fileName
  return $ buildRules readLines
