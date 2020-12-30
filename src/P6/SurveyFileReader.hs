module P6.SurveyFileReader(
  readSurveyFile,
  processSurveyFile,
  processSurveyLines
  ) where

import System.IO
import Data.List

import P6.Survey(
  AnswerGroup,
  Survey,
  buildAnswerGroup
  )

readSurveyFile :: String -> IO Survey
readSurveyFile fileName = do
  fileCts <- readFile fileName
  return $ processSurveyFile fileCts

processSurveyFile :: String -> Survey
processSurveyFile fileCts = processSurveyLines [] (lines fileCts)

processSurveyLines :: Survey -> [String] -> Survey
processSurveyLines surveySoFar surveyInLines =
  let (answerGroupLines, remainingSurveyLines) = span (not . null) surveyInLines
      answerGroup = buildAnswerGroup answerGroupLines
  in case remainingSurveyLines of
       [] -> (answerGroup : surveySoFar)
       _ : remainingSurveyLines -> processSurveyLines (answerGroup : surveySoFar) remainingSurveyLines
