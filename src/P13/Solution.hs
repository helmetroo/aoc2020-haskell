module P13.Solution(
  earliestBusTimesMinutesWaited
  ) where

import Data.List(
  minimumBy
  )

import Data.Ord(
  comparing
  )

import P13.BusNotes(
  BusNotes(..)
  )

earliestBusTimesMinutesWaited :: BusNotes -> Integer
earliestBusTimesMinutesWaited busNotes =
  let earliestBus = findEarliestBus busNotes
      departTime = earliestDepartureTime busNotes
  in earliestBus * waitingTimeForBus departTime earliestBus

findEarliestBus :: BusNotes -> Integer
findEarliestBus busNotes =
  let (departTime, buses) = (earliestDepartureTime busNotes, busIds busNotes)
  in minimumBy (comparing $ waitingTimeForBus departTime) buses

waitingTimeForBus :: Integer -> Integer -> Integer
waitingTimeForBus departureTime busId = busId - (departureTime `mod` busId)
