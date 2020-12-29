module P5.Solution(
  findHighestSeatId,
  findYourSeatId
  ) where

import P5.BoardingPass(
  BoardingPass,
  Seat(sid),
  findSeat
  )

getSeatIds :: [BoardingPass] -> [Int]
getSeatIds = map (sid . findSeat)

findHighestSeatId :: [BoardingPass] -> Int
findHighestSeatId passes = maximum $ getSeatIds passes

findYourSeatId :: [BoardingPass] -> Int
findYourSeatId passes =
  let seatIds = getSeatIds passes
      lowestSeatId = minimum seatIds
      highestSeatId = maximum seatIds
      sumOfLowestThruHighest = ((highestSeatId * (highestSeatId + 1)) `div` 2) - sum [1..lowestSeatId - 1]
      sumOfGivenSeatIds = sum seatIds
  in sumOfLowestThruHighest - sumOfGivenSeatIds
