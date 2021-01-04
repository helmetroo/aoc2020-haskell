module P8.ProgramFileReader(
  readProgramFile
  ) where

import System.IO
import Data.Sequence
import P8.Machine(
  parseInstruction,
  Program
  )

readProgramFile :: String -> IO Program
readProgramFile fileName = fromList . map parseInstruction . lines <$> readFile fileName
