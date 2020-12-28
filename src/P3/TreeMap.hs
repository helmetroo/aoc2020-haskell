module P3.TreeMap where

data Square = Tree | Space
  deriving Eq

type TreeMapRow = [Square]
type TreeMapList = [TreeMapRow]
data Point = Point {
  row :: Int,
  col :: Int
}

data Position = Position {
  pos :: Point,
  reachedEnd :: Bool
}

data Direction = Direction {
  dir :: Point
}

data TreeMap = TreeMap {
  width :: Int,
  height :: Int,
  value :: TreeMapList
}

toTreeMap :: TreeMapList -> TreeMap
toTreeMap rows = TreeMap {
  width = length (rows !! 0),
  height = length rows,
  value = rows
}

initialPosition = Position {
  pos = Point { row = 0, col = 0 },
  reachedEnd = False
}

nextPosition :: Direction -> Position -> TreeMap -> Position
nextPosition direction currentPosition treeMap =
  let mapWidth = width treeMap
      mapHeight = height treeMap
      p = pos currentPosition
      d = dir direction
      curRow = row p
      curCol = col p
      dirRow = row d
      dirCol = col d
      maxRow = mapHeight - 1
      nextCol = (curCol + dirCol) `mod` mapWidth
      nextRow = min maxRow (curRow + dirRow)
      justReachedEnd = nextRow == maxRow
  in Position {
    pos = Point { row = nextRow, col = nextCol },
    reachedEnd = justReachedEnd
  }

toSquare :: Char -> Square
toSquare char
  | char == '#' = Tree
  | otherwise   = Space

squareAt :: Position -> TreeMap -> Square
squareAt position treeMap =
  let p = pos position
      r = row p
      c = col p
      m = value treeMap
  in (m !! r) !! c
