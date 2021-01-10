module P11.SeatMapFileReader(
  readSeatMapFile
  ) where

import System.IO
import P11.SeatMap(
  SeatMap,
  toSeatMap
  )

readSeatMapFile :: String -> IO SeatMap
readSeatMapFile fileName = do
  seatMapLines <- lines <$> readFile fileName
  return $ toSeatMap seatMapLines

