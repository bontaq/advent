module DayFour.Puzzle where

import Text.Parser.Combinators
import Text.Parser.Token
import Text.Parser.Char
import Text.Parser.LookAhead
import Text.Trifecta.Parser
import Text.Trifecta.Result
import Text.Read

import Data.Maybe
import Data.List
import Control.Applicative

parseLine :: Parser String
parseLine = manyTill anyChar (try (string "\n"))

splitter strings =
  let s          = takeWhile ((> 0) . length) strings
      (s', rest) = fmap (drop 1) $ splitAt (length s) strings
  in case s' of
    []    -> Nothing
    s''   -> Just (s'', rest)

-- take in the lines, group until the blank line, then
-- combine them into a single string again for parsing
groupLines :: [String] -> [String]
groupLines strings =
  concatMap (\x -> x <> " ")
  <$> unfoldr splitter strings

parseFields :: Parser [(String, String)]
parseFields =
  many $ (,)
  <$> many letter
  <*  char ':'
  <*> manyTill anyChar space

parsePassport :: String -> [(String, String)]
parsePassport str = foldResult
  (\_ -> [])
  id
  (parseString parseFields mempty str)

parsePassports :: [String] -> [[(String, String)]]
parsePassports = fmap parsePassport

validatePassport :: [(String, String)] -> Bool
validatePassport passport =
  let requiredParts = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
      finder = \toFind -> find (\(field, val) -> field == toFind) passport
  in
    case sequence $ fmap finder requiredParts of
      Nothing -> False
      _       -> True

validatePassports :: [[(String, String)]] -> [Bool]
validatePassports = fmap validatePassport

partOne :: IO ()
partOne = do
  parsed <- fromJust <$> parseFromFile (many parseLine) "./src/DayFour/data.txt"
  let
    passports = parsePassports $ groupLines parsed
    validatedPassports = validatePassports passports
  print . show $ length $ filter (== True) validatedPassports

parseHeight :: String -> Maybe (Int, String)
parseHeight str =
  let parser = (,) <$> integer <*> many letter
  in
    foldResult
    (\_ -> Nothing)
    (\(hgt, kind) -> Just (fromInteger hgt, kind))
    (parseString parser mempty str)

parseHairColor :: String -> Bool
parseHairColor str =
  let
    digits   = concat $ fmap show [0..9]
    chars    = ['a'..'f']
    allChars = digits <> chars
    parser   = (,) <$> char '#' <*> some (oneOf allChars) <* eof
  in
    foldResult
    (const False)
    (const True)
    (parseString parser mempty str)

parsePid :: String -> Bool
parsePid str =
  let parser = count 9 digit <* eof
  in
    foldResult
    (const False)
    (const True)
    (parseString parser mempty str)

rules :: (String, String) -> Bool
rules (field, value) = case field of
  "byr" -> case (readMaybe value :: Maybe Int) of
    Nothing  -> False
    Just num -> (num >= 1920) && (num <= 2002)
  "iyr" -> case (readMaybe value :: Maybe Int) of
    Nothing  -> False
    Just num -> (num >= 2010) && (num <= 2020)
  "eyr" -> case (readMaybe value :: Maybe Int) of
    Nothing  -> False
    Just num -> (num >= 2020) && (num <= 2030)
  "hgt" -> case parseHeight value of
    Nothing -> False
    Just (height, kind) -> case kind of
      "cm" -> (height >= 150) && (height <= 193)
      "in" -> (height >= 59) && (height <= 76)
      _    -> False
  "hcl" -> parseHairColor value
  "ecl" ->
    let validColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
    in case find (== value) validColors of
      Nothing -> False
      _       -> True
  "pid" -> parsePid value
  "cid" -> True
  other -> error other

newValidatePassport :: [(String, String)] -> Bool
newValidatePassport =
  all (== True) . fmap rules

newValidatePassports :: [[(String, String)]] -> [Bool]
newValidatePassports =
  fmap newValidatePassport . filter validatePassport

partTwo :: IO ()
partTwo = do
  parsed <- fromJust <$> parseFromFile (many parseLine) "./src/DayFour/data.txt"
  let
    passports = parsePassports $ groupLines parsed
    validatedPassports = newValidatePassports passports
  print . show $ length $ filter (== True) validatedPassports
