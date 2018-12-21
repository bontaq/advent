module DaySixteen.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser (Parser, parseString, parseFromFile)
import Data.Bits
import Data.List
import Control.Lens

data Example = Example [Int] [Int] [Int]
             deriving Show

registerParse :: Parser [Integer]
registerParse = do
  char '['
  a <- token integer
  string ", "
  b <- token integer
  string ", "
  c <- token integer
  string ", "
  d <- token integer
  char ']'
  pure $ [a, b, c, d]

beforeParse :: Parser [Integer]
beforeParse = do
  string "Before: "
  registerParse

commandParse :: Parser [Integer]
commandParse = do
  a <- token integer
  whiteSpace
  b <- token integer
  whiteSpace
  c <- token integer
  whiteSpace
  d <- token integer
  pure $ [a, b, c, d]

afterParse :: Parser [Integer]
afterParse = do
  string "After:  "
  registerParse

exampleParse :: Parser Example
exampleParse = do
  before <- beforeParse
  newline
  command <- commandParse
  after <- afterParse
  newline
  optional newline
  pure $ Example
    (map fromInteger before) (map fromInteger command) (map fromInteger after)

registers = [0, 0, 0, 0]

type Command = ([Char], Int, Int, Int)
type Register = (Int, Int, Int, Int)

runCommand :: Command -> [Int] -> [Int]
runCommand (op, a, b, c) r@[a', b', c', d'] =
  case op of
    "addr" -> set (ix c) (r !! a + r !! b) r
    "addi" -> set (ix c) (r !! a + b) r

    "mulr" -> set (ix c) (r !! a * r !! b) r
    "muli" -> set (ix c) (r !! a * b) r

    "banr" -> set (ix c) (r !! a .&. r !! b) r
    "bani" -> set (ix c) (r !! a .&. b) r

    "borr" -> set (ix c) (r !! a .|. r !! b) r
    "bori" -> set (ix c) (r !! a .|. b) r

    "setr" -> set (ix c) (r !! a) r
    "seti" -> set (ix c) a r

    "gtir" -> set (ix c) (if a > r !! b then 1 else 0) r
    "gtri" -> set (ix c) (if r !! a > b then 1 else 0) r
    "gtrr" -> set (ix c) (if r !! a > r !! b then 1 else 0) r

    "eqir" -> set (ix c) (if a == r !! b then 1 else 0) r
    "eqri" -> set (ix c) (if r !! a == b then 1 else 0) r
    "eqrr" -> set (ix c) (if r !! a == r !! b then 1 else 0) r

ops = [
  "addr"
  , "addi"

  , "mulr"
  , "muli"

  , "banr"
  , "bani"

  , "borr"
  , "bori"

  , "setr"
  , "seti"

  , "gtir"
  , "gtri"
  , "gtrr"

  , "eqir"
  , "eqri"
  , "eqrr"
  ]


-- 16 opcodes
-- every instruction go:
-- opcode inputA inputB output
data Inst = Opcode

testCmd :: Example -> Int
testCmd (Example before cmd after) =
  let [op, a, b, c] = cmd
      cmds = map (\op -> (op, a, b, c)) ops
      rans = map (\cmd -> runCommand cmd before) cmds
  in length $ filter (== after) rans

partOne = do
  res <- parseFromFile (many exampleParse) "./src/DaySixteen/DataOne.txt"
  let testing = fmap (take 5) res
      tested = fmap (map testCmd) res
      counted = fmap (length . filter (>=3)) tested
  print counted

-- gotta determine which opcode = which command
-- start with [(Int, Ops)], then remove possibilities
-- for each wrong answer?

type Op = [Char]

testCmd' :: [Char] -> Example -> Bool
testCmd' op (Example before cmd after) =
  let [opN, a, b, c] = cmd
      ran = runCommand (op, a, b, c) before
  in ran == after

getOpcode :: Example -> Int
getOpcode (Example _ cmd _) = cmd !! 0

handleCountOps :: [Example] -> [([Char], Int, Bool)]
handleCountOps examples  =
  let table = map (\op -> map (\ex -> (op, getOpcode ex, testCmd' op ex)) examples) ops
  in filter (\(_, _, b) -> b == True) $ concat table

bestCandidate :: Op -> [(Op, Int, Bool)] -> Int
bestCandidate op res =
  let eqs = filter (\(op', _, _) -> op == op') res
      codes = map (\(_, code, _) -> code) eqs
      sortByLength = sortBy (\a b -> compare (length b) (length a))
  in head . head . sortByLength . group $ sort codes

partTwo = do
  res <- parseFromFile (many exampleParse) "./src/DaySixteen/DataOne.txt"
  let figureOps = fmap handleCountOps res
      best = fmap (bestCandidate "addr") figureOps

  print best
  -- print "F"
