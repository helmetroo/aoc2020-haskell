module P3.Solution(
  countTreeHits
  ) where

import P3.TreeMap(
  Square(..),
  TreeMap,
  Point(..),
  Position(..),
  Direction(..),
  initialPosition,
  nextPosition,
  squareAt
  )

countTreeHits :: TreeMap -> Direction -> Integer
countTreeHits treeMap direction =
  countTreeHitsFrom treeMap direction initialPosition 0

countTreeHitsFrom :: TreeMap -> Direction -> Position -> Integer -> Integer
countTreeHitsFrom treeMap direction position hits =
  if reachedEnd position
  then hits
  else
    let newPosition = nextPosition direction position treeMap
        squareAtNewPosition = squareAt newPosition treeMap
        squareIsTree = squareAtNewPosition == Tree
        newHits = if squareIsTree then (hits + 1) else hits
    in countTreeHitsFrom treeMap direction newPosition newHits
