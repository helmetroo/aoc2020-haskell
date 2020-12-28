module P1.TextFileReader(
  readIntList
  ) where

import System.IO

readIntList :: String -> IO [Integer]
readIntList fileName = do
  readLines <- fmap lines $ readFile fileName
  return $ map read readLines
