module P14.ProgramFileReader where

import Data.Either(
  rights
  )

import System.IO(IO)
import P14.Machine(
  parseInstruction,
  Program
  )

readProgramFile :: String -> IO Program
readProgramFile fileName = rights . map parseInstruction . lines <$> readFile fileName
