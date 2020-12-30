module P6.Survey(
  Survey,
  AnswerGroup(..),
  CharSet,
  buildAnswerGroup
  ) where

import qualified Data.Set as S

type CharSet = S.Set Char
data AnswerGroup = AnswerGroup {
  individualAnswers :: [CharSet],
  questions :: CharSet,
  questionsEveryoneAnswered :: CharSet
  }

type Survey = [AnswerGroup]

buildAnswerGroup :: [String] -> AnswerGroup
buildAnswerGroup answerGroupLines =
  let individualAnswers = map S.fromList answerGroupLines
      questions = unionAll individualAnswers
      questionsEveryoneAnswered = intersectAll individualAnswers
  in AnswerGroup {
    individualAnswers = individualAnswers,
    questions = questions,
    questionsEveryoneAnswered = questionsEveryoneAnswered
    }

unionAll :: [CharSet] -> CharSet
unionAll (x:xs) = foldl S.union x (x:xs)

intersectAll :: [CharSet] -> CharSet
intersectAll (x:xs) = foldl S.intersection x (x:xs)
