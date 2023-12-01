-- |

module DayThirteen.Puzzle where

import Data.List
import Data.List.Split

location = "./src/DayThirteen/test.txt"

-- Number x (Number x (List (Number x)))

-- Arr [Number X, Number X, Arr [Number X Number X]]

data Token
  = Open
  | Num Int
  | Close
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize "" = []
tokenize str = case head str of
  '[' -> Open : tokenize (drop 1 str)

  ']' -> case take 2 str of
    "]]" -> Close : tokenize (drop 1 str)
    "]," -> Close : tokenize (drop 2 str)
    "]"  -> Close : tokenize (drop 1 str)

  ',' -> tokenize (drop 1 str)

  _  ->
    let num = takeWhile (\c -> c /= ',' && c /= ']') str
        rest = dropWhile (\c -> c /= ',' && c /= ']') str
    in Num (read num :: Int) : tokenize rest

parsePair raw =
  let [a, b] = lines raw
  in (tokenize a, tokenize b)

pairs = do
  raw <- readFile location

  let
    rawPairs = splitOn "\n\n" raw
    pairs = fmap parsePair rawPairs

  pure pairs

data Value
  = Arr [Value]
  | Number Int
  deriving (Show, Eq)

findClose' :: [(Int, Token)] -> Int -> Int
findClose' (tok:toks) level = case tok of
  (index, Close) ->
    if level == 0 then index else findClose' toks (level - 1)
  (_, Open) ->
    findClose' toks (level + 1)
  (_, Num _) ->
    findClose' toks level

findClose :: [Token] -> Int
findClose toks = findClose' (zip [0..] toks) 0

values :: [Token] -> [Value]
values [] = []
values (tok:toks) = case tok of
  Open ->
    let
      close = findClose toks
      (it, cont) = splitAt close toks
    in
      [Arr (values it)] <> values (drop 1 cont)
  Num x -> [Number x] <> values toks

partOne = do
  p <- pairs

  let valueized = fmap (\(a, b) -> (values a, values b)) p

  mapM_ print p
  mapM_ print (valueized)
