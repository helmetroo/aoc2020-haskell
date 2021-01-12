module P12.SolutionPrinter(
  printSolution
  ) where

import P12.RouteFileReader(
  readRouteFile
  )

import P12.Solution(
  getLastManhattanDistance,
  getLastManhattanDistanceWithWaypoint
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  route <- readRouteFile $ inputFileFor 12
  print $ getLastManhattanDistance route
  print $ getLastManhattanDistanceWithWaypoint route
