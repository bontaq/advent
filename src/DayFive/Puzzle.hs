module DayFive.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust, fromMaybe)
import Control.Lens
import Debug.Trace

input = "1002,4,3,4,33"

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

data Mode = I | P
            deriving (Show, Eq)

mode '1' = I
mode '0' = P

parseOp c | trace (show c) False = undefined
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
          z = drop ((length s) - 1) s
        in
          ((read z :: Int), [mode a, mode b])
    4 ->
        let
          stupid = take (length s - 2) s
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          z = drop ((length s) - 2) s
        in
          ((read z :: Int), [mode a, mode b])
    _ -> error s


runProgram :: Int -> Vector Int -> ([Int], [Int]) -> (Vector Int, ([Int], [Int]))
runProgram offset program io | trace (show $ show offset <> " " <> show io) False = undefined
runProgram offset program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    store = program ! (offset + 3)
  in
    case op of
      99 -> (program, io)
      _  -> run
        where run = case op of
                1 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (+) v0 v1)]) io
                2 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (*) v0 v1)]) io
                3 ->
                  let ((i:is), out) = io
                  in runProgram (offset + 2) (program // [(program ! (offset + 1), i)]) (is, out)
                4 ->
                  let (i, o) = io
                      (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                  in runProgram (offset + 2) program (i, v0 : o)
                e -> error $ show offset <> " " -- <> show program

-- partOne :: IO ()
partOne = do
  f <- readFile "./src/DayFive/data.txt"
  let
    parsed = parseString numbers mempty f
    -- fixed = fmap (\x -> fromList ((take 2000 (repeat 0)) <> x <> (take 2000 (repeat 0)))) parsed
    fixed = fmap (\x -> fromList ( x )) parsed
    ran = fmap (\x -> runProgram 0 x ([1], [])) fixed

  -- ran <- fmap (runProgram 0) parsed

  -- parsed >>= \x -> pure $ (runProgram 0) (fromList x) ([1], [])

  print ran

  -- pure ()
