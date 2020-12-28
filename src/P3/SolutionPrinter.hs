module P3.SolutionPrinter(
  printSolution
  ) where

import P3.TreeMapFileReader(readTreeMap)

import P3.TreeMap(
  TreeMap,
  Direction(..),
  Point(..)
  )

import P3.Solution(countTreeHits)

import SolutionUtils.InputFile(
  inputFileFor
  )

printSolution :: IO ()
printSolution = do
  treeMap <- readTreeMap $ inputFileFor 3
  print $ getSolutionForPartTwo treeMap

r1C1 = Direction { dir = Point { row = 1, col = 1 } }
r1C3 = Direction { dir = Point { row = 1, col = 3 } }
r1C5 = Direction { dir = Point { row = 1, col = 5 } }
r1C7 = Direction { dir = Point { row = 1, col = 7 } }
r2C1 = Direction { dir = Point { row = 2, col = 1 } }

getSolutionForPartOne :: TreeMap -> Integer
getSolutionForPartOne treeMap =
  let direction = r1C3
  in countTreeHits treeMap direction

getSolutionForPartTwo :: TreeMap -> Integer
getSolutionForPartTwo treeMap =
  let directions = [r1C1, r1C3, r1C5, r1C7, r2C1]
      treeHits = map (countTreeHits treeMap) directions
  in product treeHits
