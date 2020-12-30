module P6.Solution (
  totalQuestionsAnyoneAnswered,
  totalQuestionsEveryoneAnswered
  ) where

import qualified Data.Set as S

import P6.Survey(
  Survey,
  AnswerGroup(questions, questionsEveryoneAnswered),
  CharSet
  )

computeTotal :: (AnswerGroup -> CharSet) -> Survey -> Int
computeTotal field = sum . map (S.size . field)

totalQuestionsAnyoneAnswered :: Survey -> Int
totalQuestionsAnyoneAnswered = computeTotal questions

totalQuestionsEveryoneAnswered :: Survey -> Int
totalQuestionsEveryoneAnswered = computeTotal questionsEveryoneAnswered

