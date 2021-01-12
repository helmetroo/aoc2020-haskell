module P12.Route(
  runRoute,
  initialShipState,
  initialShipWaypointState,
  Route,
  ShipResult
  ) where

import Control.Arrow((***))
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
data Ship = ShipWithoutWaypoint {
  position :: Position,
  bearing :: Integer
} | ShipWithWaypoint {
  position :: Position,
  waypoint :: Position
}

type ActiveShip = State Ship
type ShipResult = Integer

initialShipState = ShipWithoutWaypoint {
  position = (0, 0),
  bearing = 90
}

initialShipWaypointState = ShipWithWaypoint {
  position = (0, 0),
  waypoint = (10, 1)
}

move :: Action -> Ship -> Ship
move (Action E value) ship =
  case ship of
    ShipWithoutWaypoint{ position = curPos } -> ship { position = first (+ value) curPos }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = first (+ value) curWp }

move (Action W value) ship =
  case ship of
    ShipWithoutWaypoint{ position = curPos } -> ship { position = first (subtract value) curPos }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = first (subtract value) curWp }

move (Action N value) ship =
  case ship of
    ShipWithoutWaypoint{ position = curPos } -> ship { position = second (+ value) curPos }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = second (+ value) curWp }

move (Action S value) ship =
  case ship of
    ShipWithoutWaypoint{ position = curPos } -> ship { position = second (subtract value) curPos }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = second (subtract value) curWp }

move (Action L value) ship =
  case ship of
    ShipWithoutWaypoint{ bearing = curBearing } -> ship { bearing = curBearing `subBearing` value }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = rotateWaypoint (-value) curWp }

move (Action R value) ship =
  case ship of
    ShipWithoutWaypoint{ bearing = curBearing } -> ship { bearing = curBearing `addBearing` value }
    ShipWithWaypoint{ waypoint = curWp } -> ship { waypoint = rotateWaypoint value curWp }

move (Action F value) ship =
  moveForward value ship

moveForward :: Integer -> Ship -> Ship
moveForward value ship =
  case ship of
    ShipWithoutWaypoint{} ->
      case bearing ship of
        0 -> move (Action N value) ship
        90 -> move (Action E value) ship
        180 -> move (Action S value) ship
        270 -> move (Action W value) ship

    ShipWithWaypoint{ position = curPos, waypoint = curWp } ->
      ship { position = moveToWaypoint curWp value curPos }

-- A clever alt to bimap
moveToWaypoint :: Position -> Integer -> Position -> Position
moveToWaypoint (wpEast, wpNorth) value =
  (+ (wpEast * value)) *** (+ (wpNorth * value))

addBearing :: Integer -> Integer -> Integer
addBearing a b = normalizeBearing $ a + b

subBearing :: Integer -> Integer -> Integer
subBearing a b = normalizeBearing $ a - b

normalizeBearing :: Integer -> Integer
normalizeBearing = applyIf (< 0) (+ 360) . (`mod` 360)

-- Already defined surely??
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf cond f x = if cond x then f x else x

-- Trying to avoid trig functions
rotateWaypoint :: Integer -> Position -> Position
rotateWaypoint degs (wpEast, wpNorth)
  | degs == -270 || degs == 90 = (wpNorth, -wpEast)
  | degs == -180 || degs == 180 = (-wpEast, -wpNorth)
  | degs == -90 || degs == 270 = (-wpNorth, wpEast)

-- join bimap abs === bimap abs abs
manhattanDistance :: Ship -> Integer
manhattanDistance = uncurry (+) . join bimap abs . position

runRoute :: Ship -> Route -> ShipResult
runRoute ship route = evalState (applyRoute route) ship

applyRoute :: Route -> ActiveShip ShipResult
applyRoute [] =
  gets manhattanDistance

applyRoute (action : nextActions) = do
  modify $ move action
  >> applyRoute nextActions
