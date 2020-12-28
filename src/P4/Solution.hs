module P4.Solution (
  countPassportsWithRequiredFields,
  countValidPassports
  ) where

import P4.Passport(
  Passport(..),
  getRequiredValues,
  requiredValuesAreDefined,
  isValidPassport
  )

countPassportsWithRequiredFields :: [Passport] -> Int
countPassportsWithRequiredFields = length . filter requiredValuesAreDefined . map getRequiredValues

countValidPassports :: [Passport] -> Int
countValidPassports = length . filter isValidPassport
