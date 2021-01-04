module P8.SolutionPrinter(
  printSolution
  ) where

import P8.ProgramFileReader(
  readProgramFile
  )

import P8.Solution(
  getLastAccumulatorValue,
  getAccumulatorValueFromTerminatedVersion
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  program <- readProgramFile $ inputFileFor 8
  print $ getLastAccumulatorValue program
  print $ getAccumulatorValueFromTerminatedVersion program
