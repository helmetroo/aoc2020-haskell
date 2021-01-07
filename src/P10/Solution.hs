module P10.Solution(
  buildDeltaMap,
  productOfDeltas
  ) where

import Data.Map(
  adjust,
  fromList,
  Map
  )
import qualified Data.Map as Map

import Control.Applicative

type DeltaMap = Map Int Integer

initialDeltaMap = fromList [(1, 0), (3, 0)]

buildDeltaMap :: [Integer] -> DeltaMap
buildDeltaMap = buildUpDeltaMap initialDeltaMap

-- https://stackoverflow.com/questions/22268226/multiplying-the-value-within-two-maybe-monads
productOfDeltas :: DeltaMap -> Maybe Integer
productOfDeltas deltaMap = (*) <$> Map.lookup 1 deltaMap <*> Map.lookup 3 deltaMap

buildUpDeltaMap :: DeltaMap -> [Integer] -> DeltaMap
buildUpDeltaMap currentDeltaMap joltages =
  case joltages of
    [] -> currentDeltaMap
    -- Pretend the device adapter is at the end, which is always 3 higher.
    [joltage] -> incDeltaMap 3 currentDeltaMap
    (joltage : joltage2 : restJoltages) ->
      let joltageDelta = joltage2 - joltage
          newDeltaMap = case joltageDelta of
            1 -> incDeltaMap 1 currentDeltaMap
            3 -> incDeltaMap 3 currentDeltaMap
      in buildUpDeltaMap newDeltaMap (joltage2 : restJoltages)

incDeltaMap :: Int -> DeltaMap -> DeltaMap
incDeltaMap = adjust (+ 1)
