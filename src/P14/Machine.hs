module P14.Machine where

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
import Data.Bits
import Data.Map(
  Map,
  empty
  )
import qualified Data.Map as Map
import Data.Word

data MaskBit = Keep | Zero | One
  deriving Show
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
  mask = array (0, 35) [(index, Keep) | index <- [0..35]],
  memory = Map.empty
}

toBitmask :: String -> Bitmask
toBitmask str =
  let associations = zip [0..] $ (map toMaskBit . reverse) str
  in array (0, 35) associations

toMaskBit :: Char -> MaskBit
toMaskBit chr =
  case chr of
    'X' -> Keep
    '1' -> One
    '0' -> Zero

execute :: Instruction -> ProgramState -> ProgramState
execute (SetMask bitmask) = setBitmask bitmask
execute (SetMemory location value) = setMemoryValue location value

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
updateMemValue memVal (index, maskVal) =
  case maskVal of
    Keep -> memVal
    One -> memVal `setBit` index
    Zero -> memVal `clearBit` index

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

runProgram :: Program -> ProgramResult
runProgram program = evalState (run program) initialProgramState

run :: Program -> Machine ProgramResult
run [] =
  gets sumMemoryValues

run (instruction : nextInstructions) = do
  modify $ execute instruction
  >> run nextInstructions

sumMemoryValues :: ProgramState -> ProgramResult
sumMemoryValues = sum . memory
