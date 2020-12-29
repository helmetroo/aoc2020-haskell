module P5.SolutionPrinter(
  printSolution
  ) where

import P5.BoardingPassFileReader(
  readBoardingPasses
  )

import P5.Solution(
  findHighestSeatId
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  boardingPasses <- readBoardingPasses $ inputFileFor 5
  print $ findHighestSeatId boardingPasses
