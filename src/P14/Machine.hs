module P14.Machine(
  Program,
  InstructionHandler(..),
  parseInstruction,
  runProgram
  ) where

import Control.Monad.State(
  State,
  modify,
  evalState,
  gets,
  get
  )
import qualified Control.Monad.State as State

import Text.ParserCombinators.Parsec(
  ParseError,
  parse,
  try,
  (<|>),
  (<?>),
  string,
  digit,
  many1,
  oneOf
  )

import Data.Array(
  Array,
  array,
  assocs
  )
import qualified Data.Array as Array
import Data.Bits(
  setBit,
  clearBit,
  testBit
  )
import Data.Map(
  Map,
  empty
  )
import qualified Data.Map as Map
import Data.Word(
  Word64
  )

data MaskBit = Floating | Zero | One
  deriving (Show, Eq)
type Memory = Map Int MemValue
type MemValue = Word64
type Bitmask = Array Int MaskBit
data Instruction = SetMask Bitmask | SetMemory Int MemValue
  deriving Show

type Program = [Instruction]
data ProgramState = ProgramState {
  mask :: Bitmask,
  memory :: Memory
}
type Machine = State ProgramState
type ProgramResult = MemValue

initialProgramState = ProgramState {
  mask = array (0, 35) [(index, Floating) | index <- [0..35]],
  memory = Map.empty
}

toBitmask :: String -> Bitmask
toBitmask str =
  let associations = zip [0..] $ (map toMaskBit . reverse) str
  in array (0, 35) associations

class MaskBittable m where
  toMaskBit :: m -> MaskBit

instance MaskBittable Bool where
  toMaskBit val = if val then One else Zero

instance MaskBittable Char where
  toMaskBit chr = case chr of
    'X' -> Floating
    '1' -> One
    '0' -> Zero

data InstructionHandler = V1 | V2

executeWith :: InstructionHandler -> Instruction -> ProgramState -> ProgramState
executeWith _ (SetMask bitmask) = setBitmask bitmask
executeWith V1 (SetMemory location value) = setMemoryValue location value
executeWith V2 (SetMemory location value) = setMemoryValues location value

setBitmask :: Bitmask -> ProgramState -> ProgramState
setBitmask bitmask state = state { mask = bitmask }

setMemoryValue :: Int -> MemValue -> ProgramState -> ProgramState
setMemoryValue location value state =
  let (curMask, curMemory) = (mask state, memory state)
      actualValue = applyMask curMask value
      newMemory = Map.insert location actualValue curMemory
  in state { memory = newMemory }

applyMask :: Bitmask -> MemValue -> MemValue
applyMask mask attemptedValue =
  foldl updateMemValue attemptedValue (Array.assocs mask)

updateMemValue :: MemValue -> (Int, MaskBit) -> MemValue
updateMemValue memVal (index, bit) =
  case bit of
    Floating -> memVal
    One -> memVal `setBit` index
    Zero -> memVal `clearBit` index

setMemoryValues :: Int -> MemValue -> ProgramState -> ProgramState
setMemoryValues location value state =
  let (curMask, curMemory) = (mask state, memory state)
      memoryUpdates = genMemoryUpdates curMask location value
      newMemory = Map.union memoryUpdates curMemory
  in state { memory = newMemory }

genMemoryUpdates :: Bitmask -> Int -> MemValue -> Memory
genMemoryUpdates mask location value =
  let addressMask = createAddressMask mask location
      addresses = map toLocation $ permuteAddresses $ Array.elems addressMask
  in Map.fromList $ zip addresses $ repeat value

createAddressMask :: Bitmask -> Int -> Bitmask
createAddressMask mask location =
  array (0, 35) $ map (setAddressMask location) (Array.assocs mask)

setAddressMask :: Int -> (Int, MaskBit) -> (Int, MaskBit)
setAddressMask location (index, bit) =
  case bit of
    Floating -> (index, Floating)
    One -> (index, One)
    Zero -> (index, toMaskBit $ location `testBit` index)

permuteAddresses :: [MaskBit] -> [[MaskBit]]
permuteAddresses [] = []
permuteAddresses [bit] =
  case bit of
    Floating -> [[Zero], [One]]
    One -> [[One]]
    Zero -> [[Zero]]
permuteAddresses (bit : bitmask) =
  case bit of
    Floating -> map (One : ) (permuteAddresses bitmask) ++ map (Zero : ) (permuteAddresses bitmask)
    One -> map (One :) $ permuteAddresses bitmask
    Zero -> map (Zero :) $ permuteAddresses bitmask

toLocation :: [MaskBit] -> Int
toLocation bits = foldl accumBitsIntoLocation 0 $ zip [0..] $ reverse bits

accumBitsIntoLocation :: Int -> (Int, MaskBit) -> Int
accumBitsIntoLocation location (index, bit) =
  if bit == One then location `setBit` index else location

parseInstruction :: String -> Either ParseError Instruction
parseInstruction = parse instruction ""

instruction =
  try maskInstruction
  <|> try memInstruction
  <?> "Invalid instruction, must be mask = ... or mem[...] = ..."

maskInstruction = do
  string "mask = "
  mask <- many1 maskCharacter
  return $ SetMask $ toBitmask mask

maskCharacter = oneOf "X01"

memInstruction = do
  string "mem["
  location <- read <$> many1 digit
  string "] = "
  value <- read <$> many1 digit
  return $ SetMemory location value

runProgram :: InstructionHandler -> Program ->  ProgramResult
runProgram handler program =
  evalState (run handler program) initialProgramState

run :: InstructionHandler -> Program -> Machine ProgramResult
run _ [] =
  gets sumMemoryValues

run handler (instruction : nextInstructions) = do
  modify $ executeWith handler instruction
  >> run handler nextInstructions

sumMemoryValues :: ProgramState -> ProgramResult
sumMemoryValues = sum . memory
