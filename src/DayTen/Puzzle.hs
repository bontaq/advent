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

type X = Int
type Y = Int

type Field = [[(X, Y, Object)]]

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

withCoords :: [[Object]] -> [[(X, Y, Object)]]
withCoords field =
  fmap (\(y, row) ->
         fmap (\(x, item) -> (x, y, item))
         (zip [0..] row))
  (zip [0..] field)

fromResult (Success a) = a

-- each round, remove stars that are no longer visible

-- 1. neighbors 0 -> 1 -> 2 expanding circle
neighbors :: Int -> (X, Y, Object) -> Field -> [(X, Y, Object)]
neighbors = undefined

-- 2. calc baseStar toStar equation and predict it out
-- 3. replace any stars in path with Empty
-- 4. filter & sum remaining

partOne :: IO ()
partOne = do
  f <- readFile "./src/DayTen/smallData.txt"
  let
    parsed = fromResult $ parseString parseField mempty f

  -- pprint parsed
  print $ withCoords parsed

  pure ()
