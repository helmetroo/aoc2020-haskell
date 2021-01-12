module P12.Solution(
  getLastManhattanDistance,
  getLastManhattanDistanceWithWaypoint
  ) where

import P12.Route(
  runRoute,
  initialShipState,
  initialShipWaypointState,
  Route,
  ShipResult
  )

getLastManhattanDistance :: Route -> ShipResult
getLastManhattanDistance = runRoute initialShipState

getLastManhattanDistanceWithWaypoint :: Route -> ShipResult
getLastManhattanDistanceWithWaypoint = runRoute initialShipWaypointState
