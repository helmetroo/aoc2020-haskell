module P11.SeatMap(
  SeatMap(..),
  toSeatMap,
  applyOccupancyRules,
  countOccupiedSeats
  ) where

import Control.Applicative(
  liftA2
  )

import Data.Foldable(
  foldMap,
  toList
  )

import Data.Array(
  array,
  (//),
  (!),
  Array
  )

type Indices = (Int, Int)
data Tile = Floor | EmptySeat | OccupiedSeat
  deriving Eq
type SeatMapArray = Array Indices Tile
type Association = (Indices, Tile)
type AssociationList = [Association]

data SeatMap = SeatMap {
  layout :: SeatMapArray,
  bounds :: Indices,
  seats :: [Indices]
  }

instance Show Tile where
  show tile =
    case tile of
      Floor -> "."
      EmptySeat -> "L"
      OccupiedSeat -> "#"

toTile :: Char -> Tile
toTile c =
  case c of
    '.' -> Floor
    'L' -> EmptySeat

isOccupiedSeat :: Tile -> Bool
isOccupiedSeat = (== OccupiedSeat)

isEmptySeat :: Tile -> Bool
isEmptySeat = (== EmptySeat)

isSeat :: Tile -> Bool
isSeat = liftA2 (||) isOccupiedSeat isEmptySeat

adjacentIndices :: Indices -> Indices -> [Indices]
adjacentIndices (rows, cols) (row, col) =
  [(adjRow, adjCol) |
   adjRow <- [row - 1 .. row + 1],
   adjCol <- [col - 1 .. col + 1],
   adjRow >= 0 && adjCol >= 0 &&
   adjRow < rows && adjCol < cols &&
   (adjRow, adjCol) /= (row, col)]

countOccupiedSeats :: SeatMapArray -> [Indices] -> Int
countOccupiedSeats mapLayout =
  length . filter isOccupiedSeat . map (mapLayout !)

countOccupiedSeatsFrom :: SeatMapArray -> Indices -> Indices -> Int
countOccupiedSeatsFrom mapLayout mapBounds seatIndices =
  let adjacentTiles = map (mapLayout !) $ adjacentIndices mapBounds seatIndices
  in length $ filter isOccupiedSeat adjacentTiles

toSeatMap :: [String] -> SeatMap
toSeatMap seatMapLines =
  let (rows, cols) = (length seatMapLines, length $ head seatMapLines)
      assocList = toAssociationList seatMapLines (rows, cols)
      layout = array ((0, 0), (rows, cols)) assocList
      seats = [indices | (indices, tile) <- assocList, isSeat tile]
  in SeatMap { layout = layout, seats = seats, bounds = (rows, cols) }

toAssociationList :: [String] -> Indices -> AssociationList
toAssociationList seatMapLines bounds = toAssociationListFrom seatMapLines bounds 0 []

toAssociationListFrom :: [String] -> Indices -> Int -> AssociationList -> AssociationList
toAssociationListFrom [] _ _ assocList = assocList
toAssociationListFrom (currentLine : restLines) bounds row assocList =
  let (rows, cols) = bounds
      indices = zip (replicate cols row) [0..cols]
      tiles = map toTile currentLine
      assocListForRow = zip indices tiles
  in toAssociationListFrom restLines bounds (row + 1) (assocListForRow ++ assocList)

applyOccupancyRules :: SeatMap -> SeatMap
applyOccupancyRules currentMap =
  let mapLayout = layout currentMap
      mapBounds = bounds currentMap
      seatIndices = seats currentMap
      changes = foldMap (toList . changesForSeat mapLayout mapBounds) seatIndices
  in currentMap { layout = mapLayout // changes }

changesForSeat :: SeatMapArray -> Indices -> Indices -> Maybe Association
changesForSeat mapLayout mapBounds seatIndices =
 let occupiedSeats = countOccupiedSeatsFrom mapLayout mapBounds seatIndices
     seat = mapLayout ! seatIndices
 in case seat of
   EmptySeat -> if occupiedSeats == 0 then Just (seatIndices, OccupiedSeat) else Nothing
   OccupiedSeat -> if occupiedSeats >= 4 then Just (seatIndices, EmptySeat) else Nothing
