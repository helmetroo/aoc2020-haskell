module P11.SeatMap(
  SeatMap(..),
  toSeatMap,
  applyOccupancyRules,
  countOccupiedSeats,
  usingFirstRules,
  usingSecondRules
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
instance Show Tile where
  show tile =
    case tile of
      Floor -> "."
      EmptySeat -> "L"
      OccupiedSeat -> "#"

type SeatMapArray = Array Indices Tile
type Association = (Indices, Tile)
type AssociationList = [Association]

data SeatMap = SeatMap {
  layout :: SeatMapArray,
  bounds :: Indices,
  seats :: [Indices]
  }

data Direction = N | NE | E | SE | S | SW | W | NW
  deriving Enum

toIncrement :: Direction -> Indices
toIncrement dir =
  case dir of
    N  -> (-1, 0)
    NE -> (-1, 1)
    E  -> (0, 1)
    SE -> (1, 1)
    S  -> (1, 0)
    SW -> (1, -1)
    W  -> (0, -1)
    NW -> (-1, -1)

directions = [N .. NW]

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

type IndicesSearch = SeatMapArray -> Indices -> Indices -> [Indices]

adjacentIndices :: IndicesSearch
adjacentIndices _ (rows, cols) (row, col) =
  [(adjRow, adjCol) |
    adjRow <- [row - 1 .. row + 1],
    adjCol <- [col - 1 .. col + 1],
    withinBounds (rows, cols) (adjRow, adjCol) &&
    (adjRow, adjCol) /= (row, col)]

withinBounds :: Indices -> Indices -> Bool
withinBounds (rows, cols) (row, col) =
    row >= 0 && col >= 0 &&
    row < rows && col < cols

visibleSeatIndices :: IndicesSearch
visibleSeatIndices mapLayout bounds pos =
  foldMap (toList . findIndicesOfSeat bounds pos mapLayout) directions

findIndicesOfSeat :: Indices -> Indices -> SeatMapArray -> Direction -> Maybe Indices
findIndicesOfSeat bounds pos mapLayout dir =
  findIndicesOfSeatFrom bounds pos mapLayout (toIncrement dir)

findIndicesOfSeatFrom :: Indices -> Indices -> SeatMapArray -> Indices -> Maybe Indices
findIndicesOfSeatFrom bounds pos mapLayout increment =
  let newPos = addIndices increment pos
      inBounds = withinBounds bounds newPos
  in if not inBounds
     then Nothing
     else if isSeat $ mapLayout ! newPos
          then Just newPos
          else findIndicesOfSeatFrom bounds newPos mapLayout increment

addIndices :: Indices -> Indices -> Indices
addIndices a b = (fst a + fst b, snd a + snd b)

countOccupiedSeats :: SeatMapArray -> [Indices] -> Int
countOccupiedSeats mapLayout =
  length . filter isOccupiedSeat . map (mapLayout !)

countOccupiedSeatsFrom :: IndicesSearch -> SeatMapArray -> Indices -> Indices -> Int
countOccupiedSeatsFrom findIndices mapLayout mapBounds seatIndices =
  let tiles = map (mapLayout !) $ findIndices mapLayout mapBounds seatIndices
  in length $ filter isOccupiedSeat tiles

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

type ChangesApplier = SeatMapArray -> Indices -> Indices -> Maybe Association

applyOccupancyRules :: ChangesApplier -> SeatMap -> SeatMap
applyOccupancyRules applySeatChanges currentMap =
  let mapLayout = layout currentMap
      mapBounds = bounds currentMap
      seatIndices = seats currentMap
      changes = foldMap (toList . applySeatChanges mapLayout mapBounds) seatIndices
  in currentMap { layout = mapLayout // changes }

changesForSeat :: Int -> IndicesSearch -> ChangesApplier
changesForSeat occupiedCount findIndices mapLayout mapBounds seatIndices =
 let occupiedSeats = countOccupiedSeatsFrom findIndices mapLayout mapBounds seatIndices
     seat = mapLayout ! seatIndices
 in case seat of
   EmptySeat -> if occupiedSeats == 0 then Just (seatIndices, OccupiedSeat) else Nothing
   OccupiedSeat -> if occupiedSeats >= occupiedCount then Just (seatIndices, EmptySeat) else Nothing

usingFirstRules = changesForSeat 4 adjacentIndices
usingSecondRules = changesForSeat 5 visibleSeatIndices
