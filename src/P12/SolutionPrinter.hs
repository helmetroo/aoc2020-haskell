module P12.SolutionPrinter(
  printSolution
  ) where

import P12.RouteFileReader(
  readRouteFile
  )

import P12.Solution(
  getLastManhattanDistance,
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  program <- readRouteFile $ inputFileFor 12
  print $ getLastManhattanDistance program
