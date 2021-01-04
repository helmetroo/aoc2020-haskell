module P8.Machine(
  runProgram
  ) where

import Data.Map(Map)
import qualified Data.Map as Map
import Control.Monad.State
import Text.Regex.PCRE
import Text.Regex.Base

type Machine = State ProgramState
type SeenCounterVals = Map Int Bool
data Instruction = Nop | Acc Int | Jmp Int

data ProgramState = ProgramState {
  programCounter :: Int,
  accumulator :: Int,
  seenCounterVals :: SeenCounterVals
}

initialProgramState = ProgramState {
  programCounter = 0,
  accumulator = 0,
  seenCounterVals = Map.empty
}

execute :: Instruction -> ProgramState -> ProgramState
execute Nop = addToProgCounter 1
execute (Acc value) = addToProgCounter 1 . addToAccumulator value
execute (Jmp value) = addToProgCounter value

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
       "nop" -> Nop
       "acc" -> Acc value
       "jmp" -> Jmp value

runProgram :: [String] -> Int
runProgram program = evalState (run program) initialProgramState

run :: [String] -> Machine Int
run programLines = do
  curState <- get
  let curProgCounter = programCounter curState
  let seenInstruction = Map.findWithDefault False curProgCounter (seenCounterVals curState)
  if seenInstruction
    then return $ accumulator curState
    else let nextInstruction = parseInstruction $ programLines !! curProgCounter
         in modify (updateSeenCounterVals curProgCounter . execute nextInstruction)
            >> run programLines
