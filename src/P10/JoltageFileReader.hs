module P10.JoltageFileReader(
  readJoltageFile
  ) where

import System.IO
import Data.Sort(sort)

readJoltageFile :: String -> IO [Integer]
readJoltageFile fileName = do
  sortedJoltages <- sort . map read . lines <$> readFile fileName
  let deviceJoltage = 3 + maximum sortedJoltages
  return $ (0 : sortedJoltages) ++ [deviceJoltage]
