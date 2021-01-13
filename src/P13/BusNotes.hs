module P13.BusNotes(
  BusNotes(..)
  ) where

data BusNotes = BusNotes {
  earliestDepartureTime :: Integer,
  busIds :: [Integer]
}
