module P9.Solution where

import Data.List

pairCombos :: (Num a, Eq a) => [a] -> [[a]]
pairCombos list = filter ((== 2) . length) $ subsequences list

possibleSums :: (Num a, Eq a) => [a] -> [a]
possibleSums list = map sum $ pairCombos list

sumsEqualing :: (Num a, Eq a) => a -> [a] -> [a]
sumsEqualing num list = filter (== num) $ possibleSums list

sublist :: (Num a, Eq a) => Int -> Int -> [a] -> [a]
sublist from to = drop from . take to

findFirstNumberNotMatching :: (Num a, Eq a) => Int -> [a] -> Maybe a
findFirstNumberNotMatching preambleSize list =
  findFirstNumberNotMatchingInRange 0 preambleSize (length list) list

findFirstNumberNotMatchingInRange :: (Num a, Eq a) => Int -> Int -> Int -> [a] -> Maybe a
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
