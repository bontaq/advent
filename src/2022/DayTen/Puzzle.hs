-- |

module DayTen.Puzzle where

location = "./src/DayTen/real.txt"

data Op = Add Int | Noop
  deriving Show

parseOp line = case words line of
  ["addx", amt] -> Add (read amt :: Int)
  ["noop"]      -> Noop

parse = do
  raw <- lines <$> readFile location

  pure (fmap parseOp raw)


runProgram :: Int -> [Op] -> [Int]
runProgram register []       = [register]
runProgram register (op:ops) = case op of
  Add amt -> [register, register] <> runProgram (register + amt) ops
  Noop    -> [register] <> runProgram register ops

toCheck = [20, 60, 100, 140, 180, 220]

getScore registerHistory = fmap (\cycle -> (registerHistory !! (cycle -1)) * cycle) toCheck

partOne = do
  ops <- parse

  let history = runProgram 1 ops
  print (length history)
  print (getScore history)
  print (sum $ getScore history)

  pure ()

-- The left-most pixel in each row is in position 0,
-- and the right-most pixel in each row is in position 39.

roll :: [(Int, Int)] -> String
roll [] = ""
roll ((cycle, register):rest) =
  if register == cycle || register == cycle - 1 || register == cycle + 1
  then "#" <> roll rest
  else "." <> roll rest

display :: String -> [String]
display ""  = []
display str = [take 40 str] <> display (drop 40 str)

partTwo = do
  ops <- parse

  let
    history = runProgram 1 ops
    scans = concat $ repeat [0..39]
    tape = zip scans history
    raw = roll tape

  mapM_ print (display raw)

  pure ()
