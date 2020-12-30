module P6.SolutionPrinter(
  printSolution
  ) where

import P6.SurveyFileReader(
  readSurveyFile
  )

import P6.Solution(
  totalQuestionsAnyoneAnswered,
  totalQuestionsEveryoneAnswered
  )

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO()
printSolution = do
  surveys <- readSurveyFile $ inputFileFor 6
  print $ totalQuestionsAnyoneAnswered surveys
  print $ totalQuestionsEveryoneAnswered surveys
