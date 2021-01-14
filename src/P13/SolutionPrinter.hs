module P13.SolutionPrinter(
  printSolution
  ) where

import P13.BusNotesFileReader(
  readBusNotesFile,
  asBusNotes,
  asBusIdsOffsets
  )

import P13.Solution(
  earliestBusTimesMinutesWaited,
  timestampBusesDepartFromOffsets
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  busNotesFile <- readBusNotesFile $ inputFileFor 13

  let busNotes = asBusNotes busNotesFile
  print $ earliestBusTimesMinutesWaited busNotes

  let busIdsOffsets = asBusIdsOffsets busNotesFile
  print $ timestampBusesDepartFromOffsets busIdsOffsets
