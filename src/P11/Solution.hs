module P11.Solution(
  countOccupiedSeatsInFinalState
  ) where

import P11.SeatMap(
  SeatMap(..),
  applyOccupancyRules,
  countOccupiedSeats
  )

countOccupiedSeatsInFinalState :: SeatMap -> Int
countOccupiedSeatsInFinalState currentMap =
  let currentLayout = layout currentMap
      seatIndices = seats currentMap
      currentOccupiedSeats = countOccupiedSeats currentLayout seatIndices

      nextMap = applyOccupancyRules currentMap
      nextLayout = layout nextMap
      nextOccupiedSeats = countOccupiedSeats nextLayout seatIndices
  in if currentOccupiedSeats == nextOccupiedSeats
     then nextOccupiedSeats
     else countOccupiedSeatsInFinalState nextMap
