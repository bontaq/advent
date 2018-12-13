module DaySix.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token

row = do
  x <- integer
  string ", "
  y <- integer
  (x, y)

partOne = do
  values <- lines . readFile
  print ""
