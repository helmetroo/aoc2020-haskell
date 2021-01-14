module P13.BusNotes(
  BusNotesFile(..),
  BusNotes(..),
  BusIdsOffsets(..)
  ) where

data BusNotesFile = BusNotesFile {
  earliestDepartureTimeStr :: String,
  busIdsStr :: [String]
}

data BusNotes = BusNotes {
  earliestDepartureTime :: Integer,
  busIds :: [Integer]
}

data BusIdsOffsets = BusIdsOffsets {
  ids :: [Integer],
  offsets :: [Integer]
}
