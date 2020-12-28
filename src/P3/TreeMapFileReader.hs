module P3.TreeMapFileReader(
  readTreeMap
  ) where

import System.IO

import P3.TreeMap(
  TreeMap,
  TreeMapRow,
  toSquare,
  toTreeMap
  )

readTreeMap :: String -> IO TreeMap
readTreeMap fileName = do
  readLines <- fmap lines $ readFile fileName
  return $ toTreeMap $ map lineToTreeMapRow readLines

lineToTreeMapRow :: String -> TreeMapRow
lineToTreeMapRow line = map toSquare line
