module P11.Solution(
  countOccupiedSeatsInFinalState,
  Rule(..)
  ) where

import P11.SeatMap(
  SeatMap(..),
  applyOccupancyRules,
  countOccupiedSeats,
  usingFirstRules,
  usingSecondRules
  )

data Rule = First | Second

toChangesApplier rule =
  case rule of
    First -> usingFirstRules
    Second -> usingSecondRules

countOccupiedSeatsInFinalState :: Rule -> SeatMap -> Int
countOccupiedSeatsInFinalState rule currentMap =
  let currentLayout = layout currentMap
      seatIndices = seats currentMap
      currentOccupiedSeats = countOccupiedSeats currentLayout seatIndices

      applySeatChanges = toChangesApplier rule
      nextMap = applyOccupancyRules applySeatChanges currentMap
      nextLayout = layout nextMap
      nextOccupiedSeats = countOccupiedSeats nextLayout seatIndices
  in if currentOccupiedSeats == nextOccupiedSeats
     then nextOccupiedSeats
     else countOccupiedSeatsInFinalState rule nextMap
