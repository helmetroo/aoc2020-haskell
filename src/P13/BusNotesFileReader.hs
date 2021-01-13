module P13.BusNotesFileReader(
  readBusNotesFile
  ) where

import System.IO(
  IO
  )
import Text.Read(
  readMaybe
  )
import Data.Foldable(
  foldMap,
  toList
  )

import SolutionUtils.Splitter(
  splitByComma
  )

import P13.BusNotes(
  BusNotes(..)
  )

readBusNotesFile :: String -> IO BusNotes
readBusNotesFile fileName = do
  [earliestDepartureTimeStr, busIdsStr] <- lines <$> readFile fileName
  return BusNotes { earliestDepartureTime = read earliestDepartureTimeStr, busIds = parseBusIds busIdsStr }

readMaybeInt :: String -> Maybe Integer
readMaybeInt = readMaybe

parseBusIds :: String -> [Integer]
parseBusIds str =
  let lexedBusIds = splitByComma str
  in foldMap (toList . readMaybeInt) lexedBusIds
