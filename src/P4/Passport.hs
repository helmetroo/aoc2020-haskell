{-# LANGUAGE TemplateHaskell #-}

module P4.Passport(
  Passport(..),
  buildPassport,
  getRequiredValues,
  requiredValuesAreDefined
  ) where

import Data.Maybe
import Data.Label

import SolutionUtils.Splitter(
  splitBySpace,
  splitByColonIntoPair
  )

data Passport = Passport {
  _birthYear :: Maybe Int,
  _issueYear :: Maybe Int,
  _expirationYear :: Maybe Int,
  _height :: Maybe String,
  _hairColor :: Maybe String,
  _eyeColor :: Maybe String,
  _passportId :: Maybe Integer,
  _countryId :: Maybe Integer
  } deriving (Show, Eq)

mkLabel ''Passport

readInt :: String -> Maybe Int
readInt str = Just (read str :: Int)

readInteger :: String -> Maybe Integer
readInteger str = Just (read str :: Integer)

readString :: String -> Maybe String
readString = Just

modifyPassport :: String -> String -> Passport -> Passport
modifyPassport fieldAlias value passport =
  case fieldAlias of
    "byr" -> set birthYear (readInt value) passport
    "iyr" -> set issueYear (readInt value) passport
    "eyr" -> set expirationYear (readInt value) passport
    "hgt" -> set height (readString value) passport
    "hcl" -> set hairColor (readString value) passport
    "ecl" -> set eyeColor (readString value) passport
    "pid" -> set passportId (readInteger value) passport
    "cid" -> set countryId (readInteger value) passport

emptyPassport = Passport {
  _birthYear = Nothing,
  _issueYear = Nothing,
  _expirationYear = Nothing,
  _height = Nothing,
  _hairColor = Nothing,
  _eyeColor = Nothing,
  _passportId = Nothing,
  _countryId = Nothing
}
buildPassport :: String -> Passport
buildPassport kvPairsString =
  let kvPairs = splitBySpace kvPairsString
      kvPairsTuples = map splitByColonIntoPair kvPairs
  in foldl (\passport (fieldAlias, fieldValue) -> modifyPassport fieldAlias fieldValue passport) emptyPassport kvPairsTuples

requiredValues :: ([Passport -> Maybe Int], [Passport -> Maybe Integer], [Passport -> Maybe String])
requiredValues = ([
  get birthYear,
  get issueYear,
  get expirationYear
  ], [
  get passportId
  ], [
  get height,
  get hairColor,
  get eyeColor
  ])

getRequiredValues :: Passport -> ([Maybe Int], [Maybe Integer], [Maybe String])
getRequiredValues passport =
  let (intGetters, integerGetters, stringGetters) = requiredValues
      withPassport = ($ passport)
      requiredInts = map withPassport intGetters
      requiredIntegers = map withPassport integerGetters
      requiredStrings = map withPassport stringGetters
  in (requiredInts, requiredIntegers, requiredStrings)

requiredValuesAreDefined :: ([Maybe Int], [Maybe Integer], [Maybe String]) -> Bool
requiredValuesAreDefined (ints, integers, strings) =
  all isJust ints &&
  all isJust integers &&
  all isJust strings
