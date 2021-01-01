module P7.Rules(
  buildRules,
  toLabel,
  toParentBag,
  getNextChildBag,
  BagType(..),
  Rules
  ) where

import Data.Map(Map)
import qualified Data.Map as Map

import Text.Printf
import Text.Regex.PCRE
import Text.Regex.Base

data BagType = BagType {
  attribute :: String,
  color :: String
  }

data AllowedBagType = AllowedBagType {
  quantity :: Int,
  bagType :: BagType
  }

instance Show BagType where
  show bagType =
    let (a, c) = (attribute bagType, color bagType)
    in printf "%s %s bag" a c

instance Show AllowedBagType where
  show allowedBagType =
    let b = bagType allowedBagType
        (q, a, c) = (quantity allowedBagType, attribute b, color b)
    in printf "%d %s %s bag" q a c

instance Ord BagType where
  compare a b =
    let (labelA, labelB) = (toLabel a, toLabel b)
    in labelA `compare` labelB

instance Eq BagType where
  (==) a b =
    let (labelA, labelB) = (toLabel a, toLabel b)
    in labelA == labelB

type AllowedBagTypes = [AllowedBagType]
type BagContainments = Map BagType AllowedBagTypes
type BagParentNode = (String, String, [String])
type BagParentNodes = [BagParentNode]
type Rules = (BagContainments, BagParentNodes)

toLabel :: BagType -> String
toLabel bagType = (attribute bagType) ++ " " ++ (color bagType)

parentBagsPattern = "(\\w+)\\s(\\w+)\\sbags"
noChildBagsPattern = "no other bags."
childBagsPattern = "(\\d+) (\\w+) (\\w+) (?:bag|bags)(?:,|.)\\s*"

isNoChildBags :: String -> Bool
isNoChildBags rule = (rule =~ noChildBagsPattern) :: Bool

isChildBags :: String -> Bool
isChildBags rule = (rule =~ childBagsPattern) :: Bool

getNextChildBag :: String -> (String, AllowedBagType)
getNextChildBag rule =
      let (_, _, remainingRule, [strQuantity, attribute, color]) =
            rule =~ childBagsPattern :: (String, String, String, [String])
          bagType = BagType { attribute = attribute, color = color }
      in (remainingRule, AllowedBagType { quantity = read strQuantity, bagType = bagType })

toParentBag :: String -> BagType
toParentBag rule =
  let (_, _, _, [attribute, color]) =
        rule =~ parentBagsPattern :: (String, String, String, [String])
  in BagType { attribute = attribute, color = color }

buildRules :: [String] -> Rules
buildRules = foldl addContainmentsAndParents (Map.empty, [])

addContainmentsAndParents :: Rules -> String -> Rules
addContainmentsAndParents (containments, parents) rule =
  let parentBag = toParentBag rule
      childrenBags = extractChildrenBags rule []
      parentNode = toParentNode childrenBags parentBag
  in (Map.insert parentBag childrenBags containments, parentNode : parents)

extractChildrenBags :: String -> [AllowedBagType] -> [AllowedBagType]
extractChildrenBags rule seenChildrenBags
  | isNoChildBags rule = []
  | rule == "." || rule == "" = seenChildrenBags
  | isChildBags rule =
      let (remainingRule, nextChildBag) = getNextChildBag rule
      in extractChildrenBags remainingRule (nextChildBag : seenChildrenBags)

toParentNode :: AllowedBagTypes -> BagType -> BagParentNode
toParentNode children parentBag =
  case children of
    [] -> (parentLabel, parentLabel, [])
    _ -> let childrenLabels = map (toLabel . bagType) children
         in (parentLabel, parentLabel, childrenLabels)
  where parentLabel = toLabel parentBag
