module SolutionUtils.IntegerFileReader(
  readIntFile
  ) where

import System.IO

class (Integral a) => IntFileReader a where
  readIntFile :: String -> IO[a]

-- Redundant :(
instance IntFileReader Int where
  readIntFile fileName = map read <$> readLines fileName

instance IntFileReader Integer where
  readIntFile fileName = map read <$> readLines fileName

readLines :: String -> IO [String]
readLines fileName = lines <$> readFile fileName
