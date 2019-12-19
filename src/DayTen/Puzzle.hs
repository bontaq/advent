module DayTen.Puzzle where

import Text.Parser.Combinators (many, some, try, optional, eof)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char, anyChar, newline, oneOf)
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result
import Debug.Trace

data Object = Asteroid | Empty

instance Show Object where
  show Asteroid = "#"
  show Empty    = "."

objectize '.' = Empty
objectize '#' = Asteroid

parseObjects :: Parser [Object]
parseObjects = do
  i <- some (oneOf ".#")
  _ <- optional newline
  pure (fmap objectize i)

parseField :: Parser [[Object]]
parseField = many parseObjects

pprint :: [[Object]] -> IO [()]
pprint objects = mapM (putStrLn . show) objects

fromResult (Success a) = a

partOne :: IO ()
partOne = do
  f <- readFile "./src/DayTen/smallData.txt"
  let
    parsed = fromResult $ parseString parseField mempty f

  pprint parsed

  pure ()
