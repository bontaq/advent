module DaySixteen.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser (Parser, parseString, parseFromFile)

data Example = Example [Integer] [Integer] [Integer]
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
  pure $ Example before command after

registers = [0, 0, 0, 0]

-- 16 opcodes
-- every instruction go:
-- opcode inputA inputB output
data Inst = Opcode

partOne = do
  res <- parseFromFile (many exampleParse) "./src/DaySixteen/DataOne.txt"
  print res
