module P5.Solution(
  findHighestSeatId
  ) where

import P5.BoardingPass(
  BoardingPass,
  Seat(sid),
  findSeat
  )

findHighestSeatId :: [BoardingPass] -> Int
findHighestSeatId passes = maximum $ map (sid . findSeat) passes
