module P13.BusNotesFileReader(
  readBusNotesFile,
  asBusNotes,
  asBusIdsOffsets
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
  BusNotesFile(..),
  BusNotes(..),
  BusIdsOffsets(..)
  )

readBusNotesFile :: String -> IO BusNotesFile
readBusNotesFile fileName = do
  [departureTimeStr, idsStr] <- lines <$> readFile fileName
  return BusNotesFile { earliestDepartureTimeStr = departureTimeStr, busIdsStr = splitByComma idsStr }

readMaybeInt :: String -> Maybe Integer
readMaybeInt = readMaybe

parseBusIds :: [String] -> [Integer]
parseBusIds = foldMap (toList . readMaybeInt)

asBusNotes :: BusNotesFile -> BusNotes
asBusNotes busNotesFile =
  let (departureTimeStr, idsStr) = (earliestDepartureTimeStr busNotesFile, busIdsStr busNotesFile)
  in BusNotes { earliestDepartureTime = read departureTimeStr, busIds = parseBusIds idsStr }

asBusIdsOffsets :: BusNotesFile -> BusIdsOffsets
asBusIdsOffsets busNotesFile =
  let idsStr = busIdsStr busNotesFile
      (allIds, offsets) = unzip [(read idStr, offset) | (idStr, offset) <- zip idsStr [0..], idStr /= "x"]
  in BusIdsOffsets { ids = allIds, offsets = offsets }
