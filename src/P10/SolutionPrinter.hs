module P10.SolutionPrinter(
  printSolution
  ) where

import Data.Maybe

import P10.JoltageFileReader(
  readJoltageFile
  )

import P10.Solution(
  buildDeltaMap,
  productOfDeltas,
  totalAdapterArrangements
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  joltages <- readJoltageFile $ inputFileFor 10
  let deltaMap = buildDeltaMap joltages
  print $ fromMaybe 0 $ productOfDeltas deltaMap

  print $ totalAdapterArrangements joltages
