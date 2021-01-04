module P8.ProgramFileReader(
  readProgramFile
  ) where

import System.IO

readProgramFile :: String -> IO [String]
readProgramFile fileName = lines <$> readFile fileName
