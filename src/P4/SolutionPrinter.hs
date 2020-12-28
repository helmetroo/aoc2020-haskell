module P4.SolutionPrinter(
  printSolution
  ) where

import P4.PassportFileReader(
  readPassports
  )

import P4.Solution(
  countValidPassports
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  passports <- readPassports $ inputFileFor 4
  print $ countValidPassports passports
