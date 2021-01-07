module P10.JoltageFileReader(
  readJoltageFile
  ) where

import System.IO
import Data.Sort(sort)

readJoltageFile :: String -> IO [Integer]
readJoltageFile fileName =
  (0 :) .
  sort .
  map read .
  lines <$> readFile fileName
