module P7.Solution (
  bagsContainingAtLeastOneOf,
  countBagsRequiredInside
  ) where

import Data.Graph(
  graphFromEdges,
  transposeG,
  reachable
  )

import qualified Data.Map as Map
import Data.Maybe

import P7.Rules(
  BagType(..),
  AllowedBagType(..),
  AllowedBagTypes, 
  BagContainments,
  Rules,
  toLabel
  )

bagsContainingAtLeastOneOf :: BagType -> Rules -> Int
bagsContainingAtLeastOneOf bagType (_, parents) =
  let (parentChildGraph, _, vertexFromKey) = graphFromEdges parents
      childParentGraph = transposeG parentChildGraph
      bagVertex = vertexFromKey $ toLabel bagType
      reachableFromGraph = reachable childParentGraph
      allBagsContaining = reachableFromGraph <$> bagVertex
  in maybe 0 (subtract 1 . length) allBagsContaining

countBagsRequiredInside :: BagType -> Rules -> Int
countBagsRequiredInside bagType (containments, _) =
  let childBags = Map.lookup bagType containments
      totalBagsInChildren = sumTotalBagsRequiredInside containments <$> childBags
  in fromMaybe 0 totalBagsInChildren

sumTotalBagsRequiredInside :: BagContainments -> AllowedBagTypes -> Int
sumTotalBagsRequiredInside containments children =
  let totalBagsInside = totalBagsRequiredInside containments
  in sum $ map totalBagsInside children

totalBagsRequiredInside :: BagContainments -> AllowedBagType -> Int
totalBagsRequiredInside containments allowedBag =
  let (qtyAllowedBag, allowedBagType) = (quantity allowedBag, bagType allowedBag)
      childBags = Map.lookup allowedBagType containments
      totalBagsInsideChildren = sumTotalBagsRequiredInside containments <$> childBags
  in qtyAllowedBag + (qtyAllowedBag * fromMaybe 0 totalBagsInsideChildren)
