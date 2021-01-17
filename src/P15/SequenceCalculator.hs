module P15.SequenceCalculator(
  calculateSequence
  ) where

import Data.Map(
  Map,
  findWithDefault,
  fromList,
  empty,
  size,
  union
  )
import qualified Data.Map as Map

type SaidNumbers = Map Int NumStatus
data NumStatus = NumStatus {
  lastTurn :: Maybe Int,
  lastLastTurn :: Maybe Int
} deriving (Show, Eq)

defaultNumStatus :: NumStatus
defaultNumStatus = NumStatus{ lastTurn = Nothing, lastLastTurn = Nothing }

toNumStatus :: Int -> NumStatus
toNumStatus n = NumStatus{ lastTurn = Just n, lastLastTurn = Nothing }

calculateSequence :: [Int] -> Int -> Int
calculateSequence nums limit =
  let saidNumbers = Map.fromList $ zip nums $ map toNumStatus [1..]
      turn = Map.size saidNumbers + 1
      lastNumber = last nums
  in calculateSequenceFrom turn limit lastNumber saidNumbers
  where
    calculateSequenceFrom :: Int -> Int -> Int -> SaidNumbers -> Int
    calculateSequenceFrom turn limit lastNumber saidNumbers =
      let lastStatus = Map.findWithDefault defaultNumStatus lastNumber saidNumbers
          numberSpoken = case lastStatus of
            NumStatus{ lastTurn = Just lTurn, lastLastTurn = Just llTurn } -> lTurn - llTurn
            NumStatus{ lastTurn = Just _, lastLastTurn = Nothing } -> 0
            NumStatus{ lastTurn = Nothing, lastLastTurn = Nothing } -> 0

          newSaidNumbers =
            if lastNumber == numberSpoken
            then Map.insert numberSpoken NumStatus { lastTurn = Just turn, lastLastTurn = Just (turn - 1) } saidNumbers
            else
              let spokenStatus = Map.findWithDefault defaultNumStatus numberSpoken saidNumbers
                  newStatusForSpokenNumber = NumStatus{ lastTurn = Just turn, lastLastTurn = lastTurn spokenStatus }
              in Map.insert numberSpoken newStatusForSpokenNumber saidNumbers
      in if turn == limit
         then numberSpoken
         else calculateSequenceFrom (turn + 1) limit numberSpoken newSaidNumbers
