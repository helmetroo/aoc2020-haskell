module P12.RouteFileReader(
  readRouteFile
  ) where

import System.IO
import Data.Sequence
import P12.Route(
  Route
  )

readRouteFile :: String -> IO Route
readRouteFile fileName = map read . lines <$> readFile fileName
