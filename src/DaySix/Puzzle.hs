module DaySix.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, letter, newline)
import Text.Trifecta.Parser (Parser, parseFromFile)

pair :: Parser (String, String)
pair = do
  fromPlanet <- many letter
  _ <- char ')'
  toPlanet <- many letter
  _ <- optional newline
  pure (fromPlanet, toPlanet)

pairs :: Parser [(String, String)]
pairs = many pair

strand :: [(String, String)] -> [[(String, String)]]
strand connections = findCons <$> connections
  where
    findCons (planetA, planetB) =
      let match = filter (\(a,b) -> planetB == a) connections
      in case match of
        [] -> []
        _  -> undefined

partOne = do
  parsed <- parseFromFile pairs "./src/DaySix/data.txt"
  let strands = strand <$> parsed
  print strands
  print $ length $ concat $ concat strands
