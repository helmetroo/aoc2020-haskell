module P11.SolutionPrinter(
  printSolution
  ) where

import P11.Solution(
  countOccupiedSeatsInFinalState
  )

import P11.SeatMapFileReader(
  readSeatMapFile
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  seatMap <- readSeatMapFile $ inputFileFor 11
  print $ countOccupiedSeatsInFinalState seatMap
