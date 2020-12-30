module P6.Solution (
  totalQuestionsAnswered
  ) where

import qualified Data.Set as S

import P6.Survey(
  Survey,
  AnswerGroup(questions)
  )

totalQuestionsAnswered :: Survey -> Int
totalQuestionsAnswered = sum . map (S.size . questions)

