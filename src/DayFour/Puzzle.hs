module DayFour.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Trifecta.Parser

import Data.Maybe
import Control.Applicative


parseBlocks :: Parser String
parseBlocks =
  manyTill anyChar $ (choice [string "\n\n", eof >> []])

parseEndBlock :: Parser String
parseEndBlock = manyTill anyChar eof

-- parseFile :: Parser [String]
-- parseFile =
--   many parseBlocks
    -- $ try parseEndBlock
    -- <|> try parseBlocks

-- parser :: Parser [String]
-- parser =
--   many anyChar `sepBy` (string)

partOne :: IO ()
partOne = do
  parsed <- fromJust <$> parseFromFile parseBlocks "./src/DayFour/data.txt"
  print . show $ take 1 $ parsed
