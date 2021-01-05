module P9.NumberFileReader(
  readNumberFile
  ) where

import System.IO

readNumberFile :: String -> IO [Integer]
readNumberFile fileName = map read . lines <$> readFile fileName
