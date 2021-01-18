module P16.Solution(
  scanErrorRate
  ) where

import Control.Applicative(
  liftA2
  )

import P16.TicketNotes(
  TicketNotes(..),
  FieldRange,
  FieldRanges
  )

scanErrorRate :: TicketNotes -> Int
scanErrorRate notes = sum invalidValues
  where invalidValues =
          let (nearby, ranges) = (concat $ nearbyTickets notes, map snd $ schema notes)
          in filter (not . inRanges ranges) nearby

inRanges :: [FieldRanges] -> Int -> Bool
inRanges ranges num = any (`inRange` num) ranges

inRange :: FieldRanges -> Int -> Bool
inRange (range1, range2) = liftA2 (||) (within range1) (within range2)

within :: FieldRange -> Int -> Bool
within (a, b) = liftA2 (&&) (>= a) (<= b)
