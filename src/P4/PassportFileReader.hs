module P4.PassportFileReader(
  readPassports,
  processPassports,
  processPassportLines
  ) where

import System.IO
import Data.List

import P4.Passport(
  Passport(..),
  buildPassport
  )

readPassports :: String -> IO [Passport]
readPassports fileName = do
  fileCts <- readFile fileName
  return $ processPassports fileCts

processPassports :: String -> [Passport]
processPassports fileCts = processPassportLines [] (lines fileCts)

processPassportLines :: [Passport] -> [String] -> [Passport]
processPassportLines passports passportsInLines =
  let (passportArray, passportLines) = span (not . null) passportsInLines
      passport = (buildPassport . intercalate " ") passportArray
  in case passportLines of
       [] -> (passport : passports)
       _ : remainingPassportLines -> processPassportLines (passport : passports) remainingPassportLines
