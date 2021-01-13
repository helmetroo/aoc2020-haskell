module P13.SolutionPrinter(
  printSolution
  ) where

import P13.BusNotesFileReader(
  readBusNotesFile
  )

import P13.Solution(
  earliestBusTimesMinutesWaited
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  busNotes <- readBusNotesFile $ inputFileFor 13

  print $ earliestBusTimesMinutesWaited busNotes
