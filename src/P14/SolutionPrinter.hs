module P14.SolutionPrinter(
  printSolution
  ) where

import P14.ProgramFileReader(
  readProgramFile
  )

import P14.Machine(
  InstructionHandler(..),
  runProgram
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  program <- readProgramFile $ inputFileFor 14
  print $ runProgram V1 program
  print $ runProgram V2 program
