module P13.Solution(
  earliestBusTimesMinutesWaited,
  timestampBusesDepartFromOffsets
  ) where

import Data.List(
  minimumBy,
  find
  )
import Data.Maybe(
  fromMaybe
  )
import Data.Ord(
  comparing
  )

import P13.BusNotes(
  BusNotes(..),
  BusIdsOffsets(..)
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

timestampBusesDepartFromOffsets :: BusIdsOffsets -> Integer
timestampBusesDepartFromOffsets busIdsOffsets =
  let (allIds, offs) = (ids busIdsOffsets, offsets busIdsOffsets)
      idsOffsets = zip allIds offs
      idsProduct = product allIds
      crSum = foldl (chineseRemainderSum idsProduct) 0 idsOffsets
  in ((`mod` idsProduct) . (+ idsProduct) . negate) crSum

chineseRemainderSum :: Integer -> Integer -> (Integer, Integer) -> Integer
chineseRemainderSum idsProduct curSum (busId, offset) =
  let productWithoutBusId = idsProduct `div` busId
      (_, _, modInv) = extendedGCD busId productWithoutBusId
  in curSum + (offset * modInv * productWithoutBusId)

-- extended GCD adapted from https://shainer.github.io/crypto/math/2017/10/22/chinese-remainder-theorem.html
extendedGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extendedGCD x y = extendedGCDFrom 1 x y (1, 0) (0, 1)

extendedGCDFrom :: Integer -> Integer -> Integer -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer, Integer)
extendedGCDFrom q x y (x0, x1) (y0, y1) =
  let (nq, nx, ny) = (x `div` y, y, x `mod` y)
      (nx0, nx1) = (x1, x0 - nq * x1)
      (ny0, ny1) = (y1, y0 - nq * y1)
  in if ny > 0
     then extendedGCDFrom nq nx ny (nx0, nx1) (ny0, ny1)
     else (nq, nx0, ny0)
