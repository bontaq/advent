module DayEight.Puzzle where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Text.Trifecta.Parser

import Data.Maybe
import Data.List
import Control.Applicative
import Control.Lens

data Instruction
  = Nop Integer
  | Acc Integer
  | Jmp Integer
  deriving Show

parseInstruction :: Parser Instruction
parseInstruction = do
  instruction <- some letter <* space
  num <- integer
  pure $ case instruction of
    "nop" -> Nop num
    "acc" -> Acc num
    "jmp" -> Jmp num

partOne = do
  parsed <- fromJust <$> parseFromFile (many parseInstruction) "./src/DayEight/data.txt"
  print $ runInstructions' parsed
  print (last parsed)

type Pointer = Integer
type Acc = Integer

runInstructions :: Acc -> Pointer -> [(Instruction, Bool)] -> Acc
runInstructions acc pointer instructions =
  let
    (instruction, ran) = instructions !! (fromInteger pointer)
    newInstructions = instructions & ix (fromInteger pointer) .~ (instruction, True)
  in
    if ran
    then acc
    else
      case instruction of
        Nop _      -> runInstructions acc (pointer + 1) newInstructions
        Jmp to     -> runInstructions acc (pointer + to) newInstructions
        Acc adjust -> runInstructions (acc + adjust) (pointer + 1) newInstructions


runInstructions' instructions =
  let withRunStatus = flip zip (repeat False)
  in runInstructions 0 0 (withRunStatus instructions)
-- so we have an accumulator and a location -- instructions should be marked if run

innerCheck :: Acc -> Pointer -> [(Instruction, Bool)] -> (Bool, Acc)
innerCheck acc pointer instructions =
  let
    (instruction, ran) = instructions !! (fromInteger pointer)
    newInstructions = instructions & ix (fromInteger pointer) .~ (instruction, True)
  in
    if ran
    then (False, acc)
    else
      case instruction of
        Nop _        -> outerCheck acc (pointer + 1) newInstructions
        (Jmp to)     -> outerCheck acc (pointer + to) newInstructions
        (Acc adjust) -> outerCheck (acc + adjust) (pointer + 1) newInstructions

outerCheck :: Acc -> Pointer -> [(Instruction, Bool)] -> (Bool, Acc)
outerCheck acc pointer instr =
  if pointer > ((toInteger $ length instr) - 1)
  then (True, acc)
  else innerCheck acc pointer instr

realCheck :: [(Instruction, Bool)] -> [(Bool, Acc)]
realCheck instructions =
  foldr (\((instr, ran), i) acc -> handle instr i acc) [] (zip instructions [0..])
  where
    -- one nop to jmp
    -- one jmp to nop
    handle (Nop adjust) i acc =
      let adjusted = instructions & ix i .~ (Jmp adjust, False)
      in outerCheck 0 0 adjusted : acc
    handle (Jmp adjust) i acc =
      let adjusted = instructions & ix i .~ (Nop adjust, False)
      in outerCheck 0 0 adjusted : acc
    handle (Acc adjust) i acc = acc

runInstructions'' instructions =
  let withRunStatus = flip zip (repeat False)
  in realCheck (withRunStatus instructions)

partTwo = do
  parsed <- fromJust <$> parseFromFile (many parseInstruction) "./src/DayEight/data.txt"
  print $ runInstructions'' parsed
  print (last parsed)
