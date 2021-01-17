module P15.SolutionPrinter(
  printSolution
  ) where

import SolutionUtils.IntegerFileReader(
  readIntFile
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

import P15.SequenceCalculator(
  calculateSequence
  )

printSolution :: IO()
printSolution = do
  nums <- readIntFile $ inputFileFor 15
  print $ calculateSequence nums 2020
  print $ calculateSequence nums 30000000
