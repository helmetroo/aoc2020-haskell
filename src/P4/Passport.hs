{-# LANGUAGE TemplateHaskell #-}

module P4.Passport(
  Passport(..),
  buildPassport,
  getRequiredValues,
  requiredValuesAreDefined,
  isValidPassport
  ) where

import Data.Maybe
import Data.Label
import Text.Regex.PCRE
import Text.Regex.Base

import SolutionUtils.Splitter(
  splitBySpace,
  splitByColonIntoPair
  )

data HeightUnit = Centimeters | Inches | Invalid
data Height = Height {
  _value :: Int,
  _unit :: HeightUnit
}

toHeightUnit :: String -> HeightUnit
toHeightUnit str
  | str == "cm" = Centimeters
  | str == "in" = Inches
  | otherwise = Invalid

data Passport = Passport {
  _birthYear :: Maybe Int,
  _issueYear :: Maybe Int,
  _expirationYear :: Maybe Int,
  _height :: Maybe Height,
  _hairColor :: Maybe String,
  _eyeColor :: Maybe String,
  _passportId :: Maybe String,
  _countryId :: Maybe String
  }

mkLabel ''Height
mkLabel ''Passport

readInt :: String -> Maybe Int
readInt str = Just (read str :: Int)

readString :: String -> Maybe String
readString = Just

readHeight :: String -> Maybe Height
readHeight str =
  let (_,_,_,lexedHeight) = str =~ "(\\d+)(cm|in)" :: (String, String, String, [String])
  in case lexedHeight of
    [] -> Nothing
    _ : [] -> Nothing
    value : unit : [] -> Just $ Height (read value) (toHeightUnit unit)

modifyPassport :: String -> String -> Passport -> Passport
modifyPassport fieldAlias value passport =
  case fieldAlias of
    "byr" -> set birthYear (readInt value) passport
    "iyr" -> set issueYear (readInt value) passport
    "eyr" -> set expirationYear (readInt value) passport
    "hgt" -> set height (readHeight value) passport
    "hcl" -> set hairColor (readString value) passport
    "ecl" -> set eyeColor (readString value) passport
    "pid" -> set passportId (readString value) passport
    "cid" -> set countryId (readString value) passport

between :: (Num a, Ord a) => a -> a -> a -> Bool
between mini maxi val = (mini <= val) && (val <= maxi)

validHeight :: Height -> Bool
validHeight height =
  let (h, u) = (get value height, get unit height)
  in
    case u of
      Centimeters -> between 150 193 h
      Inches -> between 59 76 h
      _ -> False

validColor :: String -> Bool
validColor color = (color =~ "#[a-f0-9]{6}") :: Bool

validEyeColor :: String -> Bool
validEyeColor color = color `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validId :: String -> Bool
validId = (== 9) . length

validateBirthYear passport = maybe False (between 1920 2002) (get birthYear passport)
validateIssueYear passport = maybe False (between 2010 2020) (get issueYear passport)
validateExpirationYear passport = maybe False (between 2020 2030) (get expirationYear passport)
validateHeight passport = maybe False validHeight (get height passport)
validateHairColor passport = maybe False validColor (get hairColor passport)
validateEyeColor passport = maybe False validEyeColor (get eyeColor passport)
validatePassportId passport = maybe False validId (get passportId passport)

isValidPassport :: Passport -> Bool
isValidPassport passport =
  all (== True) [validateBirthYear passport,
                 validateIssueYear passport,
                 validateExpirationYear passport,
                 validateHeight passport,
                 validateHairColor passport,
                 validateEyeColor passport,
                 validatePassportId passport]

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

requiredValues :: (
  [Passport -> Maybe Int],
  [Passport -> Maybe Height],
  [Passport -> Maybe String]
  )
requiredValues = ([
  get birthYear,
  get issueYear,
  get expirationYear
  ], [
  get height
  ], [
  get passportId,
  get hairColor,
  get eyeColor
  ])

getRequiredValues :: Passport -> (
  [Maybe Int],
  [Maybe Height],
  [Maybe String]
  )
getRequiredValues passport =
  let (intGetters, heightGetters, stringGetters) = requiredValues
      withPassport = ($ passport)
      requiredInts = map withPassport intGetters
      requiredHeights = map withPassport heightGetters
      requiredStrings = map withPassport stringGetters
  in (requiredInts, requiredHeights, requiredStrings)

requiredValuesAreDefined :: ([Maybe Int], [Maybe Height], [Maybe String]) -> Bool
requiredValuesAreDefined (ints, heights, strings) =
  all isJust ints &&
  all isJust heights &&
  all isJust strings
