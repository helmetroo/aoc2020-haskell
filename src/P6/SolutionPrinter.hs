module P6.SolutionPrinter(
  printSolution
  ) where

import P6.SurveyFileReader(
  readSurveyFile
  )

import P6.Solution(
  totalQuestionsAnswered
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  surveys <- readSurveyFile $ inputFileFor 6
  print $ totalQuestionsAnswered surveys
