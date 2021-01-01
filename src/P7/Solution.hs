module P7.Solution (
  totalBagsContaining
  ) where

import Data.Graph(
  graphFromEdges,
  transposeG,
  reachable
  )

import P7.Rules(
  BagType(..),
  Rules,
  toLabel
  )

totalBagsContaining :: BagType -> Rules -> Int
totalBagsContaining bagType (containments, parents) =
  let (parentChildGraph, _, vertexFromKey) = graphFromEdges parents
      childParentGraph = transposeG parentChildGraph
      bagVertex = vertexFromKey $ toLabel bagType
      reachableFromBagRulesGraph = reachable childParentGraph
      allBagsContaining = reachableFromBagRulesGraph <$> bagVertex
  in maybe 0 (subtract 1 . length) allBagsContaining
