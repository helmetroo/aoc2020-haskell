module P12.Solution(
  getLastManhattanDistance,
  ) where

import P12.Route(
  runRoute,
  Route,
  ShipResult
  )

getLastManhattanDistance :: Route -> ShipResult
getLastManhattanDistance = runRoute
