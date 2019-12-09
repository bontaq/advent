module DayFive.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust)

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

parseOp :: Int -> (Int, [Mode])
parseOp i =
  let t  = show i
      op = (\s -> read s :: Int) $ drop (length t - 2) t
      p  = take (length t - 2) t

      mode '1' = I
      mode '0' = P
  in (op, fmap mode p)

runProgram :: Int -> Vector Int -> ([Int], [Int]) -> ([Int], [Int])
runProgram offset program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    v0 = program ! (program ! (offset + 1))
    v1 = program ! (program ! (offset + 2))

    store = program ! (offset + 3)
  in
    case op of
      99 -> io
      _  -> run
        where run = case op of
                1 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m2 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (+) v0 v1)]) io
                2 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m2 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (*) v0 v1)]) io
                3 ->
                  let ((i:is), out) = io
                  in runProgram (offset + 2) (program // [(program ! (offset + 1), i)]) (is, out)
                4 ->
                  let (i, o) = io
                  in runProgram (offset + 2) program (i, v0 : o)
                e -> error $ show program

-- partOne :: IO ()
partOne = do
  let
    parsed = parseString numbers mempty input
    -- fixed = fmap ((\x -> x // [(1, 12), (2, 2)]) . fromList) parsed
    -- ran = fmap (runProgram 0) fixed

  -- ran <- fmap (runProgram 0) parsed

  parsed >>= \x -> pure $ (runProgram 0) (fromList x) ([1], [])

  -- pure ()
