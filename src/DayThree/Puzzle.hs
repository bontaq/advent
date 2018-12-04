{-# LANGUAGE QuasiQuotes #-}

module DayThree.Puzzle (partOne) where

import Text.RawString.QQ
import Text.Parser.Combinators (manyTill, many)
import Text.Parser.Char (char, anyChar, newline, string)
import Text.Parser.Token (token, integer)
import Text.Trifecta.Parser (Parser, parseFromFile)

exampleInput = [r|
#1 @ 861,330: 20x10
#2 @ 491,428: 28x23
#3 @ 64,746: 20x27
#4 @ 406,769: 25x28
|]

-- x and y start from the top left
-- width extends right
-- height extends down
data Claim = Claim { _id     :: Integer
                   , _x      :: Integer
                   , _y      :: Integer
                   , _width  :: Integer
                   , _height :: Integer
                   } deriving (Show)

row :: Parser Claim
row = do
  char '#'
  claim <- token integer
  string "@ "
  x <- token integer
  char ','
  y <- token integer
  char ':' <* char ' '
  width <- token integer
  char 'x'
  height <- token integer

  pure $ Claim claim x y width height

partOne = do
  res <- parseFromFile (many row) "./src/DayThree/Data.txt"
  print res
