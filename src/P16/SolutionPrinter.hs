module P16.SolutionPrinter(
  printSolution
  ) where

import System.IO(
  IO,
  hPrint,
  stderr
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

import P16.TicketNotesParser(
  parseTicketNotes
  )

import P16.Solution(
  scanErrorRate
  )

printSolution :: IO()
printSolution = do
  notesFile <- readFile $ inputFileFor 16
  either (hPrint stderr . ("Parse notes error: " ++) . show)
    (print . scanErrorRate)
    (parseTicketNotes notesFile)

