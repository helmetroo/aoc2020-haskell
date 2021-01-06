module P9.Solution(
  findFirstNumberNotMatching,
  findEncryptionWeakness
  ) where

import Data.List

pairCombos :: (Num a, Eq a, Ord a) => [a] -> [[a]]
pairCombos list = filter ((== 2) . length) $ subsequences list

possibleSums :: (Num a, Eq a, Ord a) => [a] -> [a]
possibleSums list = map sum $ pairCombos list

sumsEqualing :: (Num a, Eq a, Ord a) => a -> [a] -> [a]
sumsEqualing num list = filter (== num) $ possibleSums list

sublist :: (Num a, Eq a, Ord a) => Int -> Int -> [a] -> [a]
sublist from to = drop from . take to

findFirstNumberNotMatching :: (Num a, Eq a, Ord a) => Int -> [a] -> Maybe a
findFirstNumberNotMatching preambleSize list =
  findFirstNumberNotMatchingInRange 0 preambleSize (length list) list

findFirstNumberNotMatchingInRange :: (Num a, Eq a, Ord a) => Int -> Int -> Int -> [a] -> Maybe a
findFirstNumberNotMatchingInRange from to listLength list =
  case inBounds of
    True -> let currentPreamble = sublist from to list
                currentNumber = list !! to
                sumsEqualingNumber = sumsEqualing currentNumber currentPreamble
            in case sumsEqualingNumber of
                 [] -> Just currentNumber
                 _ -> findFirstNumberNotMatchingInRange (from + 1) (to + 1) listLength list
    False -> Nothing
  where inBounds = to < listLength

findEncryptionWeakness :: (Num a, Eq a, Ord a) => [a] -> a -> a
findEncryptionWeakness list target =
  let listSummingToTarget = getSublistSummingTo list target
  in (minimum listSummingToTarget + maximum listSummingToTarget)

getSublistSummingTo :: (Num a, Eq a, Ord a) => [a] -> a -> [a]
getSublistSummingTo list target =
  let (start, end) = findSublistBoundsSumsTo target 0 0 0 list list
  in sublist start (end + 1) list

findSublistBoundsSumsTo :: (Num a, Eq a, Ord a) => a -> a -> Int -> Int -> [a] -> [a] -> (Int, Int)
findSublistBoundsSumsTo target currentSum start end list iteratingList =
  case iteratingList of
    [] -> (start, end)
    (nextValue : leftIteratingList) ->
      let (nextSum, newStart, newList) = cutFromSublistUntilBelowTarget target (currentSum + nextValue) start list
      in nextBoundsOrFinish target nextSum newStart end newList leftIteratingList

cutFromSublistUntilBelowTarget :: (Num a, Eq a, Ord a) => a -> a -> Int -> [a] -> (a, Int, [a])
cutFromSublistUntilBelowTarget target currentSum start list =
  case list of
    [] -> (currentSum, start, [])
    (x : rest) -> if currentSum > target
                  then cutFromSublistUntilBelowTarget target (currentSum - x) (start + 1) rest
                  else (currentSum, start, x : rest)

nextBoundsOrFinish :: (Num a, Eq a, Ord a) => a -> a -> Int -> Int -> [a] -> [a] -> (Int, Int)
nextBoundsOrFinish target nextSum start end list remainingList =
  if nextSum == target
  then (start, end)
  else findSublistBoundsSumsTo target nextSum start (end + 1) list remainingList
