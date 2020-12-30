{-# LANGUAGE TemplateHaskell #-}

module P6.Survey(
  Survey,
  AnswerGroup(..),
  buildAnswerGroup
  ) where

import qualified Data.Set as S

type CharSet = S.Set Char
data AnswerGroup = AnswerGroup {
  questions :: CharSet,
  size :: Int
  }

type Survey = [AnswerGroup]

buildAnswerGroup :: [String] -> AnswerGroup
buildAnswerGroup answerGroupLines =
  let questions = (S.fromList . concat) answerGroupLines
      size = length answerGroupLines
  in AnswerGroup { questions = questions, size = size }
