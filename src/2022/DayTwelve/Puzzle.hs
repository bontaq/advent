-- |

module DayTwelve.Puzzle where

import Prelude hiding (map, round)
import Control.Lens
import Data.Maybe
import Data.List
import qualified Data.Set as Set

{-

The inevitable pathing problem

Wellp, spider out!

-}

type X = Int
type Y = Int
type Point = ((X, Y), Char)
type Map = [[Point]]

location = "./src/DayTwelve/test.txt"

geography = lines <$> readFile location

heights = zip ['a'..'z'] [0..] <> [('S', 0), ('E', 26)]

getHeight c =
  snd $ fromJust $ find (\(c', _) -> c' == c) heights

getPoints (x, y) =
  [ (x,     y - 1)
  , (x,     y + 1)
  , (x - 1, y    )
  , (x + 1, y    )
  ]

getPointsOnMap :: [(X, Y)] -> Map -> [Point]
getPointsOnMap points map =
  mapMaybe (\(x, y) -> map ^? element y . element x) points

removeTall curHeight =
  filter (\(_, c) -> getHeight c <= curHeight + 1)

moves :: Point -> Map -> [Point]
moves ((x, y), _) map =
  let
    currentPoint = map !! y !! x
    currentHeight = getHeight (snd currentPoint)
    surrounding = getPoints (x, y)
  in
    removeTall currentHeight $ getPointsOnMap surrounding map

buildMap :: [String] -> Map
buildMap raw =
  fmap (\(y, row) ->
          fmap (\(x, c) -> ((x, y), c)) (zip [0..] row))
  (zip [0..] raw)

findStart :: Map -> Point
findStart map = fromJust $ find (\(_, c) -> c == 'S') (concat map)

advance :: Point -> Map -> [[Point]]
advance point map =
  let visit = moves point map
  in fmap (\next -> [point, next]) visit

innerRound :: [Point] -> Map -> [[Point]]
innerRound trail map =
  let advances = advance (last trail) map
  in fmap (init trail <>) advances

sortPaths [] good seen = (seen, good)
sortPaths (path:paths) good seen =
  if Set.member (last path) seen
  then sortPaths paths good (Set.union seen (Set.fromList path))
  else sortPaths paths (good <> [path]) (Set.insert (last path) seen)

round :: [[Point]] -> Set.Set Point -> Map -> (Set.Set Point, [[Point]])
round trails seen map =
  let
    paths = concatMap (`innerRound` map) trails
    (seen', saw) = sortPaths paths [] seen
--    saw = filter (\path -> not (Set.member (last path) seen)) paths
--    seen' = Set.union seen (Set.fromList (fmap last saw))
  in (seen', saw)

findPath :: [[Point]] -> Set.Set Point -> Map -> [[Point]]
findPath trails seen map =
  let (seen', paths) = round trails seen map
  in
    if any (\(_, c) -> c == 'E') (fmap last paths)
    then paths
    else findPath paths seen' map

display points =
  let board = [line | _ <- [0..50], let line = replicate 180 '.']
  in foldr (\((x, y), _) board -> set (element y . element x) '#' board) board points

partOne = do
  field <- geography

  let
    map = buildMap field
    start = findStart map

  print (findStart map)
  print (moves start map)
  print (round [[start]] Set.empty map)
  -- mapM_ print (round (round (round [[start]] map) map) map)
  print "------"

  let
    trails = findPath [[start]] Set.empty map
  -- mapM_ print trails
  -- print (length $ head trails)
  mapM_ print (display (head trails))

-- 473 is too high
-- 300 is too low
