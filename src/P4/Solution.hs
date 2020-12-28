module P4.Solution (
  countValidPassports
  ) where

import P4.Passport(
  Passport(..),
  getRequiredValues,
  requiredValuesAreDefined
  )

countValidPassports :: [Passport] -> Int
countValidPassports = length . filter requiredValuesAreDefined . map getRequiredValues
