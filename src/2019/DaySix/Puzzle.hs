module DaySix.Puzzle where

import Text.Parser.Combinators (many, try, optional, choice)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, letter, newline, digit)
import Text.Trifecta.Parser (Parser, parseFromFile)
import Data.Maybe (isJust, fromJust)

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

findMatchInner :: (String, String) -> Orbit -> Maybe Orbit
findMatchInner target p@(Planet b orbits) =
  let
    match = target == b
  in
    case match of
      True  -> Just p
      False ->
        let
          results = fmap (findMatchInner target) orbits
          filtered = filter isJust results
        in
          case length filtered of
            0 -> Nothing
            1 -> head filtered

findMatch :: Orbit -> Orbit -> Maybe (String, String)
findMatch (Planet a orbits) orbitB@(Planet b orbits') =
  let
    r = findMatchInner a orbitB
  in
    case r of
      Just _  -> Just a
      Nothing ->
        let
          results = fmap (flip findMatch orbitB) orbits
          filtered = filter isJust results
        in
          case length filtered of
            0 -> Nothing
            1 -> head filtered

strand' :: String -> [(String, String)] -> Orbit
strand' start connections = findCons $ findStart start connections
  where
    findCons (planetA, planetB) =
      let match = filter (\(a,b) -> planetA == b) connections
      in case match of
        [] -> Planet (planetA, planetB) []
        _  -> Planet (planetA, planetB) (findCons <$> match)

determineLength :: Int -> (String, String) -> Orbit -> Int
determineLength count target p@(Planet b orbits) =
  let
    match = target == b
  in
    case match of
      True  -> count
      False ->
        let
          results = fmap (determineLength (count + 1) target) orbits
          filtered = sum results
        in
          case filtered of
            0 -> 0
            count' -> count'

partTwo = do
  parsed <- parseFromFile pairs "./src/DaySix/data.txt"
  let santa = (strand' "SAN") <$> parsed
      you   = (strand' "YOU") <$> parsed
      match = fromJust $ findMatch <$> santa <*> you

  print $ (determineLength 0) <$> match <*> santa
  print $ (determineLength 0) <$> match <*> you
  print $ findMatch <$> santa <*> you
