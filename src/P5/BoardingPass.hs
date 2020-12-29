{-# LANGUAGE TemplateHaskell #-}

module P5.BoardingPass(
  BoardingPass(..),
  Seat(..),
  toBoardingPass,
  findSeat
  ) where

import Prelude hiding (Left, Right)
import Data.Label
import Text.Regex.PCRE
import Text.Regex.Base

data Regions = Front | Back | Left | Right
data Region = Upper Regions | Lower Regions | Invalid

data BoardingPass = BoardingPass {
  _rows :: [Region],
  _columns :: [Region]
}

type Bounds = (Int, Int)

data Seat = Seat {
  row :: Int,
  column :: Int,
  sid :: Int
} 

mkLabel ''BoardingPass

maxPosition :: Int -> Int
maxPosition x = (2 ^ x) - 1

toBoardingPass :: String -> BoardingPass
toBoardingPass str =
  let (_,_,_,[rowRegionChars, colRegionChars]) = str =~ "([F|B]+)([L|R]+)" :: (String, String, String, [String])
      rowRegions = map toRegion rowRegionChars
      colRegions = map toRegion colRegionChars
  in BoardingPass { _rows = rowRegions, _columns = colRegions }

toRegion :: Char -> Region
toRegion c
  | c == 'F' = Lower Front
  | c == 'B' = Upper Back
  | c == 'L' = Lower Left
  | c == 'R' = Upper Right
  | otherwise = Invalid

findSeat :: BoardingPass -> Seat
findSeat pass =
  let (rs, cs) = (get rows pass, get columns pass)
      (maxRowPos, maxColPos) = (maxPosition $ length rs, maxPosition $ length cs)
      (rowBounds, columnBounds) = ((0, maxRowPos), (0, maxColPos))
      (foundRow, foundColumn) = (findPositionFrom rowBounds rs, findPositionFrom columnBounds cs)
  in Seat { row = foundRow, column = foundColumn, sid = foundRow * 8 + foundColumn }

findPositionFrom :: Bounds -> [Region] -> Int
findPositionFrom bounds regions =
  case regions of
    [] -> 0

    [lastRegion] ->
      case lastRegion of
        Lower r -> fst bounds
        Upper r -> snd bounds
        Invalid -> 0

    nextRegion : remainingRegions ->
      let nextBounds =
            case nextRegion of
              Lower r -> takeLowerHalf bounds
              Upper r -> takeUpperHalf bounds
              Invalid -> bounds
      in findPositionFrom nextBounds remainingRegions

takeLowerHalf :: Bounds -> Bounds
takeLowerHalf (lowerBound, upperBound) =
  (lowerBound, (upperBound + lowerBound) `div` 2)

takeUpperHalf :: Bounds -> Bounds
takeUpperHalf (lowerBound, upperBound) =
  (((lowerBound + upperBound) `div` 2) + 1, upperBound)
