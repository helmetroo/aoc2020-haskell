module P2.PasswordTypes(
  Policy,
  LengthPolicy(..),
  PositionPolicy(..),
  Password,
  TokenizedPolicy,
  PasswordAndTokenizedPolicy,
  tokenizePolicy,
  toLengthPolicy,
  toPositionPolicy,
  validate
  ) where

import SolutionUtils.Splitter(splitByDash)

type Password = String
type TokenizedPolicy = (Char, Integer, Integer)
type PasswordAndTokenizedPolicy = (Password, TokenizedPolicy)

countInstancesOf :: Char -> String -> Integer
countInstancesOf _ [] = 0
countInstancesOf char (c : cs)
  | c == char = 1 + countInstancesOf char cs
  | otherwise = countInstancesOf char cs

isBetween :: Integer -> Integer -> Integer -> Bool
isBetween low hi num = num >= low && num <= hi

tokenizePolicy :: [String] -> TokenizedPolicy
tokenizePolicy line =
  let policyChar = (line !! 1) !! 0
      policyIndexList = splitByDash (line !! 0)
      firstIndex = read $ (policyIndexList !! 0)
      secondIndex = read $ (policyIndexList !! 1)
  in (policyChar, firstIndex, secondIndex)

class Policy p where
  validate :: p -> Password -> Bool

data LengthPolicy = LengthPolicy {
  minChars :: Integer,
  maxChars :: Integer,
  lCharacter :: Char
} deriving Show

data PositionPolicy = PositionPolicy {
  firstPosition :: Int,
  lastPosition :: Int,
  pCharacter :: Char
} deriving Show

toLengthPolicy :: TokenizedPolicy -> LengthPolicy
toLengthPolicy (c, minC, maxC) =
  LengthPolicy { minChars = minC, maxChars = maxC, lCharacter = c }

toPositionPolicy :: TokenizedPolicy -> PositionPolicy
toPositionPolicy (c, firstC, lastC) =
    PositionPolicy { firstPosition = fromIntegral firstC, lastPosition = fromIntegral lastC, pCharacter = c }

instance Policy LengthPolicy where
  validate policy password = 
    let minC = minChars policy
        maxC = maxChars policy
        c = lCharacter policy
        instancesOfChar = countInstancesOf c password
    in isBetween minC maxC instancesOfChar

instance Policy PositionPolicy where
  validate policy password =
    let firstIndex = (firstPosition policy) - 1
        lastIndex = (lastPosition policy) - 1
        c = pCharacter policy
    in onlyOneCharacterAtPosition (firstIndex, lastIndex) password c 

onlyOneCharacterAtPosition :: (Int, Int) -> String -> Char -> Bool
onlyOneCharacterAtPosition (firstIndex, lastIndex) password c =
  let inFirst = (password !! firstIndex) == c
      inLast = (password !! lastIndex) == c
  in (inFirst && (not inLast)) || ((not inFirst) && inLast)
