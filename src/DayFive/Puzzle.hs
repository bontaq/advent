module DayFive.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust)

input = ""

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

runProgram :: Int -> Vector Int -> Vector Int
runProgram offset program =
  let
    opcode = program ! offset
    v0 = program ! (program ! (offset + 1))
    v1 = program ! (program ! (offset + 2))
    store = program ! (offset + 3)
  in
    case opcode of
      99 -> program
      _  -> runProgram (offset + 4) (program // [(store, op v0 v1)])
        where op = case opcode of
                1 -> (+)
                2 -> (*)

partOne :: IO ()
partOne = do
  let
    parsed = parseString numbers mempty input
    fixed = fmap ((\x -> x // [(1, 12), (2, 2)]) . fromList) parsed
    ran = fmap (runProgram 0) fixed

  print ran


partOne = undefined
