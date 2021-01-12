module P12.Route where

import Control.Monad(
  join
  )
import Control.Monad.State
import Data.Bifunctor(
  bimap,
  first,
  second
  )

data Direction = N | S | E | W | L | R | F
  deriving (Show, Enum, Read)

data Action = Action Direction Integer
type Position = (Integer, Integer)

instance Show Action where
  show (Action direction value) =
    show direction ++ show value

instance Read Action where
  readsPrec _ (directionStr : valueStr) =
    let direction = read [directionStr] :: Direction
        value = read valueStr :: Integer
    in [(Action direction value, [])]

type Route = [Action]

-- Bearing direction is clockwise from north, making East 90 degs.
data Ship = Ship {
 position :: Position,
 bearing :: Integer
 }

type ActiveShip = State Ship
type ShipResult = Integer

initialShipState = Ship {
  position = (0, 0),
  bearing = 90
}

move :: Action -> Ship -> Ship
move (Action E value) ship = ship { position = first (+ value) $ position ship }
move (Action W value) ship = ship { position = first (subtract value) $ position ship }
move (Action N value) ship = ship { position = second (+ value) $ position ship }
move (Action S value) ship = ship { position = second (subtract value) $ position ship }
move (Action L value) ship = ship { bearing = bearing ship `subBearing` value }
move (Action R value) ship = ship { bearing = bearing ship `addBearing` value }
move (Action F value) ship = moveForward value ship

moveForward :: Integer -> Ship -> Ship
moveForward value ship =
  case bearing ship of
    0 -> move (Action N value) ship
    90 -> move (Action E value) ship
    180 -> move (Action S value) ship
    270 -> move (Action W value) ship

addBearing :: Integer -> Integer -> Integer
addBearing a b = normalizeBearing $ a + b

subBearing :: Integer -> Integer -> Integer
subBearing a b = normalizeBearing $ a - b

normalizeBearing :: Integer -> Integer
normalizeBearing = applyIf (< 0) (+ 360) . (`mod` 360)

-- Already defined surely??
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf cond f x = if cond x then f x else x

-- join bimap abs === bimap abs abs
manhattanDistance :: Ship -> Integer
manhattanDistance = uncurry (+) . join bimap abs . position

runRoute :: Route -> ShipResult
runRoute route = evalState (applyRoute route) initialShipState

applyRoute :: Route -> ActiveShip ShipResult
applyRoute [] =
  gets manhattanDistance

applyRoute (action : nextActions) = do
  modify $ move action
  >> applyRoute nextActions
