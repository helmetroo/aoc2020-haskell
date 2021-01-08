module P10.Solution(
  buildDeltaMap,
  productOfDeltas,
  totalAdapterArrangements
  ) where

import Prelude hiding (lookup)
import Data.Map(
  adjust,
  lookup,
  Map
  )
import qualified Data.Map as Map

import Control.Applicative

type DeltaMap = Map Int Integer
data JoltagePath = JoltagePath {
  joltage :: Integer,
  paths :: Integer
  }
type JoltagePaths = [JoltagePath]

initialDeltaMap = Map.fromList [(1, 0), (3, 0)]

buildDeltaMap :: [Integer] -> DeltaMap
buildDeltaMap = buildUpDeltaMap initialDeltaMap

-- https://stackoverflow.com/questions/22268226/multiplying-the-value-within-two-maybe-monads
productOfDeltas :: DeltaMap -> Maybe Integer
productOfDeltas deltaMap = (*) <$> lookup 1 deltaMap <*> lookup 3 deltaMap

buildUpDeltaMap :: DeltaMap -> [Integer] -> DeltaMap
buildUpDeltaMap currentDeltaMap joltages =
  case joltages of
    [] -> currentDeltaMap
    [_] -> currentDeltaMap
    (joltage : joltage2 : restJoltages) ->
      let joltageDelta = joltage2 - joltage
          newDeltaMap = incDeltaMap (fromIntegral joltageDelta) currentDeltaMap
      in buildUpDeltaMap newDeltaMap (joltage2 : restJoltages)

incDeltaMap :: Int -> DeltaMap -> DeltaMap
incDeltaMap = adjust (+ 1)

totalAdapterArrangements :: [Integer] -> Integer
totalAdapterArrangements joltages =
  let joltagesAndPaths = map toJoltagePath joltages
  in countTotalAdapterArrangements joltagesAndPaths

toJoltagePath :: Integer -> JoltagePath
toJoltagePath joltage = JoltagePath {
  joltage = joltage,
  paths = if joltage == 0 then 1 else 0
}

-- Thanks for the hint u/Nunki3 :) (https://www.reddit.com/r/adventofcode/comments/kacdbl/2020_day_10c_part_2_no_clue_how_to_begin/gf9lzhd?utm_source=share&utm_medium=web2x&context=3)
countTotalAdapterArrangements :: JoltagePaths -> Integer
countTotalAdapterArrangements [] = 0
countTotalAdapterArrangements [joltageAndPath] = paths joltageAndPath
countTotalAdapterArrangements (joltageAndPath : remainingJoltagesAndPaths) =
  let candidates = take 3 remainingJoltagesAndPaths
      candidatesWithNewPaths = updatePaths joltageAndPath candidates
  in countTotalAdapterArrangements (candidatesWithNewPaths ++ drop 3 remainingJoltagesAndPaths)

updatePaths :: JoltagePath -> JoltagePaths -> JoltagePaths
updatePaths current = map (addToJoltagePath current)

addToJoltagePath :: JoltagePath -> JoltagePath -> JoltagePath
addToJoltagePath current candidate =
  let (curJoltage, curPaths) = (joltage current, paths current)
      (canJoltage, canPaths) = (joltage candidate, paths candidate)
      isReachable = reachable curJoltage canJoltage
  in candidate { paths = if isReachable then canPaths + curPaths else canPaths }

reachable :: Integer -> Integer -> Bool
reachable a b = (b - a) <= 3
