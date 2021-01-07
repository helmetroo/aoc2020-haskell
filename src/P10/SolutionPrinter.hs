module P10.SolutionPrinter(
  printSolution
  ) where

import Data.Maybe

import P10.JoltageFileReader(
  readJoltageFile
  )

import P10.Solution(
  buildDeltaMap,
  productOfDeltas
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  numbers <- readJoltageFile $ inputFileFor 10
  let deltaMap = buildDeltaMap numbers
  print $ fromMaybe 0 $ productOfDeltas deltaMap
