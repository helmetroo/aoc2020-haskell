module P1.Solution (
  productOfTwoEntriesSummingTo2020,
  productOfThreeEntriesSummingTo2020
  ) where

twoEntriesSummingTo2020 :: [Integer] -> (Integer, Integer)
twoEntriesSummingTo2020 entries =
  [ (first, second) | first <- entries, second <- entries, first + second == 2020 ] !! 0

threeEntriesSummingTo2020 :: [Integer] -> (Integer, Integer, Integer)
threeEntriesSummingTo2020 entries =
  [ (first, second, third) | first <- entries, second <- entries, third <- entries, first + second + third == 2020 ] !! 0

productOfTwoEntriesSummingTo2020 :: [Integer] -> Integer
productOfTwoEntriesSummingTo2020 list =
  let (first, second) = twoEntriesSummingTo2020 list
  in first * second

productOfThreeEntriesSummingTo2020 :: [Integer] -> Integer
productOfThreeEntriesSummingTo2020 list =
  let (first, second, third) = threeEntriesSummingTo2020 list
  in first * second * third
