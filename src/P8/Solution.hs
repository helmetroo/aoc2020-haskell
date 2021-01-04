module P8.Solution(
  getLastAccumulatorValue,
  getAccumulatorValueFromTerminatedVersion
  ) where

import Data.List
import Data.Sequence

import P8.Machine(
  runProgram,
  swap,
  isNopOrJmp,
  Instruction(..),
  Program,
  ProgramResult(..)
  )

getLastAccumulatorValue :: Program -> Int
getLastAccumulatorValue = finalAccumulatorValue . runProgram

getAccumulatorValueFromTerminatedVersion :: Program -> Int
getAccumulatorValueFromTerminatedVersion program =
  maybe 0 finalAccumulatorValue $ resultFromTerminatedVersion program

resultFromTerminatedVersion :: Program -> Maybe ProgramResult
resultFromTerminatedVersion program =
  let indexesOfNopOrJmp = findIndicesL isNopOrJmp program
      programsWithSwappedInstructions = map (\index -> adjust swap index program) indexesOfNopOrJmp
  in findInProgramsWithSwappedInstructions programsWithSwappedInstructions

findInProgramsWithSwappedInstructions :: [Program] -> Maybe ProgramResult
findInProgramsWithSwappedInstructions [] = Nothing
findInProgramsWithSwappedInstructions (program : programs) =
  let result = runProgram program
  in
    if terminated result
    then Just result
    else findInProgramsWithSwappedInstructions programs
