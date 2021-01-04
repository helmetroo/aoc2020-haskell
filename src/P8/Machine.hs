module P8.Machine(
  runProgram,
  parseInstruction,
  swap,
  isNopOrJmp,
  Instruction(..),
  Program,
  ProgramResult(..)
  ) where

import Prelude hiding (length)
import Data.Sequence(
  Seq,
  length,
  index
  )

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.State
import Text.Regex.PCRE
import Text.Regex.Base

type Machine = State ProgramState
type SeenCounterVals = Map Int Bool
data Instruction = Nop Int | Acc Int | Jmp Int
type Program = Seq Instruction

data ProgramState = ProgramState {
  programCounter :: Int,
  accumulator :: Int,
  seenCounterVals :: SeenCounterVals
}

data ProgramResult = ProgramResult {
  finalAccumulatorValue :: Int,
  terminated :: Bool
  }

initialProgramState = ProgramState {
  programCounter = 0,
  accumulator = 0,
  seenCounterVals = Map.empty
}

execute :: Instruction -> ProgramState -> ProgramState
execute (Nop _) = addToProgCounter 1
execute (Acc value) = addToProgCounter 1 . addToAccumulator value
execute (Jmp value) = addToProgCounter value

swap :: Instruction -> Instruction
swap (Nop value) = Jmp value
swap (Jmp value) = Nop value
swap (Acc value) = Acc value

isNopOrJmp :: Instruction -> Bool
isNopOrJmp i =
  case i of
    Nop _ -> True
    Jmp _ -> True
    Acc _ -> False

addToProgCounter :: Int -> ProgramState -> ProgramState
addToProgCounter value state = state {
  programCounter = (programCounter state) + value
}

addToAccumulator :: Int -> ProgramState -> ProgramState
addToAccumulator value state = state {
  accumulator = (accumulator state) + value
}

updateSeenCounterVals :: Int -> ProgramState -> ProgramState
updateSeenCounterVals counter state = state {
  seenCounterVals = Map.insert counter True (seenCounterVals state)
}

parseInstruction :: String -> Instruction
parseInstruction line =
  let (_,_,_,[operand,argSign,argValue]) = line =~ "(nop|acc|jmp) (\\+|\\-)(\\d*)" :: (String, String, String, [String])
      intValue = read argValue :: Int
      value = if argSign == "+" then intValue else (-intValue)
  in case operand of
       "nop" -> Nop value
       "acc" -> Acc value
       "jmp" -> Jmp value

runProgram :: Program -> ProgramResult
runProgram program = evalState (run program) initialProgramState

run :: Program -> Machine ProgramResult
run instructions = do
  curState <- get
  let curProgCounter = programCounter curState
  let seenInstruction = Map.findWithDefault False curProgCounter (seenCounterVals curState)
  if seenInstruction || curProgCounter >= (length instructions)
    then return ProgramResult { finalAccumulatorValue = (accumulator curState), terminated = not seenInstruction }
    else let nextInstruction = index instructions curProgCounter
         in modify (updateSeenCounterVals curProgCounter . execute nextInstruction)
            >> run instructions
