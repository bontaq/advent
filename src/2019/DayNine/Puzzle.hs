module DayNine.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy)
import Control.Lens
import Debug.Trace

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

data Mode = R | I | P
            deriving (Show, Eq)

mode '2' = R
mode '1' = I
mode '0' = P

parseOp :: Int -> (Int, [Mode])
parseOp c =
  let s = show c
  in case length s of
    1 -> (read s :: Int, [P, P])
    2 -> (read s :: Int, [P, P])
    3 ->
        let
          stupid = "0" <> (take (length s - 1) s)
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          c = fromMaybe '0' $ stupid ^? element 2
          z = drop ((length s) - 1) s
        in
          ((read z :: Int), [mode a, mode b, mode c])
    4 ->
        let
          stupid = take (length s - 2) s
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          c = fromMaybe '0' $ stupid ^? element 2
          z = drop ((length s) - 2) s
        in
          ((read z :: Int), [mode a, mode b, mode c])
    5 ->
      let
        stupid = take ((length s) - 2) s
        c = fromMaybe '0' $ stupid ^? element 0
        a = fromMaybe '0' $ stupid ^? element 1
        b = fromMaybe '0' $ stupid ^? element 2
        z = drop ((length s) - 2) s
      in
        ((read z :: Int), [mode a, mode b, mode c])
    _ -> error s


type Done = Bool
type Offset = Int
type Relative = Int
type Adjust = Int
type ProgramOut = (Done, Offset, Program, IO')

                      -- v0 = case m2 of
                      --   P -> program ! (program ! (offset + 1))
                      --   I -> program ! (offset + 1)
                      --   R -> program ! (program ! ((offset + 1) + relative))


getValue :: Mode -> Program -> Offset -> Adjust -> Relative -> Int
getValue m program offset adjust relative = case m of
  R -> program ! ((program ! (offset + adjust)) + relative)
  I -> program ! (offset + adjust)
  P -> program ! (program ! (offset + adjust))

getWrite :: Mode -> Program -> Offset -> Adjust -> Relative -> Int
getWrite m program offset adjust relative = case m of
  R -> (program ! (offset + adjust)) + relative
  _ -> program ! (offset + adjust)

runProgram :: Int -> Int -> Vector Int -> ([Int], [Int]) -> ProgramOut
-- runProgram offset relative program io | trace (show $ (show $ program ! offset) <> "::" <> show relative) False = undefined
runProgram offset relative program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    store = program ! (offset + 3)
  in
    case op of
      99 -> (True, offset, program, io)
      _  -> run
        where run = case op of
                -- addition
                1 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                  in
                    runProgram (offset + 4) relative (program // [(store, (+) v0 v1)]) io

                -- multiply
                2 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                  in
                    runProgram (offset + 4) relative (program // [(store, (*) v0 v1)]) io

                -- store input
                3 ->
                  let (ins, out) = io
                      (m1:m2:m3:_) = modes
                  in case length ins of
                    0 -> (False, offset, program, io) -- return it for continuing to run
                    _ -> let (i:is) = ins
                         in
                          case m2 of
                            R -> runProgram (offset + 2) relative (program // [(relative + (program ! (offset + 1)), i)]) (is, out)
                            _ -> runProgram (offset + 2) relative (program // [(program ! (offset + 1), i)]) (is, out)

                -- output
                4 ->
                  let (i, o) = io
                      (m1:m2:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                  in runProgram (offset + 2) relative program (i, v0 : o)

                -- jump if true
                5 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 /= 0
                    then runProgram v1 relative program io
                    else runProgram (offset + 3) relative program io

                -- jump if false
                6 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                        -- R -> program ! (program ! ((offset + 2) + relative))
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == 0
                    then runProgram v1 relative program io
                    else runProgram (offset + 3) relative program io

                -- less than
                7 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 < v1
                    then runProgram (offset + 4) relative (program // [(store, 1)]) io
                    else runProgram (offset + 4) relative (program // [(store, 0)]) io

                -- equals
                8 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                        -- R -> program ! (program ! ((offset + 2) + relative))
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == v1
                    then runProgram (offset + 4) relative (program // [(store, 1)]) io
                    else runProgram (offset + 4) relative (program // [(store, 0)]) io

                9 ->
                  let (m1:m2:_) = modes
                      -- v0 = program ! (program
                      v0 = getValue m2 program offset 1 relative
                  in
                    runProgram (offset + 2) (relative + v0) program io

                e -> error $ show offset <> " " <> show op <> " " <> show program

type IO' = ([Int], [Int])
type Program = Vector Int

partOne = do
  f <- readFile "./src/DayNine/data.txt"
  let
    parsed = parseString numbers mempty f
    -- fixed = fmap (\x -> fromList ((take 2000 (repeat 0)) <> x <> (take 2000 (repeat 0)))) parsed
    fixed = fmap (\x -> fromList ( x <> (take 1000 (repeat 0)) )) parsed
    ran = fmap (\x -> runProgram 0 0 x ([1], [])) fixed

  print ran

partTwo = do
  f <- readFile "./src/DayNine/data.txt"
  let
    parsed = parseString numbers mempty f
    -- fixed = fmap (\x -> fromList ((take 2000 (repeat 0)) <> x <> (take 2000 (repeat 0)))) parsed
    fixed = fmap (\x -> fromList ( x <> (take 1000 (repeat 0)) )) parsed
    ran = fmap (\x -> runProgram 0 0 x ([2], [])) fixed

  print ran
