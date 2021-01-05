module P9.SolutionPrinter(
  printSolution
  ) where

import P9.NumberFileReader(
  readNumberFile
  )

import P9.Solution(
  findFirstNumberNotMatching
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  numbers <- readNumberFile $ inputFileFor 9
  print $ findFirstNumberNotMatching 25 numbers
