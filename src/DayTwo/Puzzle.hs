module DayTwo.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust)

input = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,1,19,9,23,1,23,6,27,2,27,13,31,1,10,31,35,1,10,35,39,2,39,6,43,1,43,5,47,2,10,47,51,1,5,51,55,1,55,13,59,1,59,9,63,2,9,63,67,1,6,67,71,1,71,13,75,1,75,10,79,1,5,79,83,1,10,83,87,1,5,87,91,1,91,9,95,2,13,95,99,1,5,99,103,2,103,9,107,1,5,107,111,2,111,9,115,1,115,6,119,2,13,119,123,1,123,5,127,1,127,9,131,1,131,10,135,1,13,135,139,2,9,139,143,1,5,143,147,1,13,147,151,1,151,2,155,1,10,155,0,99,2,14,0,0"

number :: Parser Integer
number = do
  i <- integer
  _ <- optional (char ',')
  pure i

numbers :: Parser [Integer]
numbers = many number

runProgram :: Int -> Vector Integer -> Vector Integer
runProgram offset program =
  let
    opcode =  program ! offset
  in
    case opcode of
      1 ->
        let
          p0 = program ! (offset + 1)
          v0 = program ! (fromIntegral p0)
          p1 = program ! (offset + 2)
          v1 = program ! (fromIntegral p1)
          store = program ! (offset + 3)
        in
          runProgram (offset + 4) $ (program // [(fromIntegral store, v0 + v1)])
      2 ->
        let
          p0 = program ! (offset + 1)
          v0 = program ! (fromIntegral p0)
          p1 = program ! (offset + 2)
          v1 = program ! (fromIntegral p1)
          store = program ! (offset + 3)
        in
          runProgram (offset + 4) $ (program // [(fromIntegral store, v0 * v1)])
      _ -> program

partOne :: IO ()
partOne = do
  let
    parsed = parseString numbers mempty input
    fixed = fmap ((\x -> x // [(1, 12), (2, 2)]) . fromList) parsed
    ran = fmap (runProgram 0) fixed

  print ran

runPossiblities :: [(Integer, Integer)] -> Vector Integer -> Integer
runPossiblities ((a, b):xs) program =
     let fixed = (\x -> x // [(1, a), (2, b)]) program
         ran = (runProgram 0) fixed
     in
       case ran ! 0 of
         19690720 -> 100 * a + b
         _ -> runPossiblities xs program

partTwo :: IO ()
partTwo = do
  let
    parsed = parseString numbers mempty input
    combinations = [(a, b) | a <- [0..99], b <- [0..99]]

  print . show  $ fmap (runPossiblities combinations . fromList) parsed
