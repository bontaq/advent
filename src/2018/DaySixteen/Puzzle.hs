module DaySixteen.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser (Parser, parseString, parseFromFile)
import Data.Bits
import Data.List (sortBy, group, sort)
import Data.Map (lookup, fromList)
import Data.Maybe (fromJust)
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
    _ -> error op

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

bestCandidate :: Op -> [(Op, Int, Bool)] -> [[Int]]
bestCandidate op res =
  let eqs = filter (\(op', _, _) -> op == op') res
      codes = map (\(_, code, _) -> code) eqs
      sortByLength = sortBy (\a b -> compare (length b) (length a))
  in sortByLength . group $ sort codes
known = [
  (6, "muli")
  , (9,  "addi")
  , (12, "addr")
  , (2,  "bori")
  , (11, "borr")
  , (3,  "mulr")
  , (8,  "setr")
  , (4,  "seti")
  , (5,  "bani")
  , (1,  "banr")
  , (10, "gtir")
  , (13, "eqrr")
  , (15, "eqir")
  , (14, "gtri")
  , (7,  "gtrr")
  , (0,  "eqri")
  ]

removeKnown :: [(String, Int, Bool)] -> [(String, Int, Bool)]
removeKnown examples =
  let knownOpCodes = map fst known
  in filter (\(_, e, _) -> not $ elem e knownOpCodes) examples

figuringOutWhichCodes = do
  res <- parseFromFile (many exampleParse) "./src/DaySixteen/DataOne.txt"
  let figureOps = fmap handleCountOps res
      cleaned = fmap removeKnown figureOps
      best = fmap (\raw -> map (\op -> (op, bestCandidate op raw)) ops) cleaned
      singulars = fmap (\raw -> filter (\(op, candidates) -> (length candidates) == 1) raw) best

  print singulars

runCommands :: [[Integer]] -> [Int]
runCommands cmds =
  let cmds' = map (map fromInteger) cmds
      knownMap = fromList known
      cmdLookup c = fromJust $ Data.Map.lookup c knownMap
  in foldr (\[op, a, b, c] register -> runCommand (cmdLookup op, a, b, c) register) [0, 0, 0, 0] (reverse cmds')

smallTest = "6 0 0 3\n9 3 2 3\n"
-- and in this one, we run the program
partTwo = do
  res <- parseFromFile (many commandParse) "./src/DaySixteen/DataTwo.txt"
  let go = fmap runCommands res
      test = runCommands [[6, 0, 0, 3], [9, 3, 2, 3]]
  -- res >>= pure $ runCommands
  print go
  print test
