module DaySeven.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token
import Text.Trifecta.Parser

testLine = "bright indigo bags contain 4 shiny turquoise bags, 3 wavy yellow bags."
-- dotted turquoise bags contain 3 vibrant salmon bags, 2 dotted maroon bags, 1 bright beige bag, 1 drab white bag.

data Bag = Bag {
  color :: String
  }

parseBag :: Parser [String]
parseBag = count 2 (some letter <* space) <* string "bags" <* oneOf " ,."

data Rule = Rule {
  bag :: Bag
  , contains :: [Bag]
}

parseRule :: Parser [(Integer, [String])]
parseRule = do
  firstBag <- parseBag
  _ <- string "contain "
  bags <- some ((,) <$> integer <*> parseBag)
  eof
  pure bags
