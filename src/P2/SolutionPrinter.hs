module P2.SolutionPrinter where

import P2.PasswordTypes(
  toLengthPolicy,
  toPositionPolicy
  )
import P2.PasswordFileReader(readPasswordsAndPolicies)

import P2.Solution(countValidPasswords)

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO ()
printSolution = do
  (passwords, tokenizedPolicies) <- readPasswordsAndPolicies $ inputFileFor 2
  let lengthPolicies = map toLengthPolicy tokenizedPolicies
  let positionPolicies = map toPositionPolicy tokenizedPolicies
  print $ countValidPasswords passwords lengthPolicies
  print $ countValidPasswords passwords positionPolicies
