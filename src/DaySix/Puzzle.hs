module DaySix.Puzzle where

import Text.Parser.Combinators (many, try, optional, choice)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, letter, newline, digit)
import Text.Trifecta.Parser (Parser, parseFromFile)

pair :: Parser (String, String)
pair = do
  fromPlanet <- many (choice [letter, digit])
  _ <- char ')'
  orbittingPlanet <- many (choice [letter, digit])
  _ <- optional newline
  pure (fromPlanet, orbittingPlanet)

pairs :: Parser [(String, String)]
pairs = many pair

strand :: [(String, String)] -> [[(String, String)]]
strand connections = findCons <$> connections
  where
    findCons (planetA, planetB) =
      let match = filter (\(a,b) -> planetB == a) connections
      in case match of
        [] -> [(planetA, planetB)]
        _  -> (planetA, planetB) : concat (findCons <$> match)

partOne = do
  parsed <- parseFromFile pairs "./src/DaySix/data.txt"
  let strands = strand <$> parsed
  print $ length $ concat $ concat strands

findStart :: String -> [(String, String)] -> (String, String)
findStart s = head . filter (\(a,b) -> b == s)

data Orbit = Planet (String, String) [Orbit]
             deriving Show

findMatch :: Orbit -> Orbit -> Maybe (String, String)
findMatch (Planet a orbits) (Planet b orbits') =
  let
    match = a == b
  in
    if a == b then Just a
    else undefined

strand' :: String -> [(String, String)] -> Orbit
strand' start connections = findCons $ findStart start connections
  where
    findCons (planetA, planetB) =
      let match = filter (\(a,b) -> planetA == b) connections
      in case match of
        [] -> Planet (planetA, planetB) []
        _  -> Planet (planetA, planetB) (findCons <$> match)

partTwo = do
  parsed <- parseFromFile pairs "./src/DaySix/smallData.txt"
  let santa = (strand' "SAN") <$> parsed
      you   = (strand' "YOU") <$> parsed
      match = fmap (findMatch you) santa
      -- filter down to just the "YOU" and "SANTA" chains
  print santa
  print you
  -- print $ length $ concat $ strands
