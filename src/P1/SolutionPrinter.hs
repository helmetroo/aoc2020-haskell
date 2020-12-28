module P1.SolutionPrinter(
  printSolution
  ) where

import P1.TextFileReader(
  readIntList
  )

import P1.Solution(
  productOfTwoEntriesSummingTo2020,
  productOfThreeEntriesSummingTo2020
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  intList <- readIntList $ inputFileFor 1
  print $ productOfTwoEntriesSummingTo2020 intList
  print $ productOfThreeEntriesSummingTo2020 intList
