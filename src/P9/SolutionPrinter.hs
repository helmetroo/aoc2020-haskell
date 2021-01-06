module P9.SolutionPrinter(
  printSolution
  ) where

import P9.NumberFileReader(
  readNumberFile
  )

import P9.Solution(
  findFirstNumberNotMatching,
  findEncryptionWeakness
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  numbers <- readNumberFile $ inputFileFor 9

  let firstNumNotMatching = findFirstNumberNotMatching 25 numbers
  print firstNumNotMatching

  print $ findEncryptionWeakness numbers <$> firstNumNotMatching
