module P14.SolutionPrinter(
  printSolution
  ) where

import P14.ProgramFileReader(
  readProgramFile
  )

import P14.Machine(
  runProgram
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  program <- readProgramFile $ inputFileFor 14
  print $ runProgram program
