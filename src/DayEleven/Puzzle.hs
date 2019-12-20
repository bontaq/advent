module DayEleven.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy)
import Control.Lens
import Debug.Trace

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

data Mode = R | I | P
            deriving (Show, Eq)

mode '2' = R
mode '1' = I
mode '0' = P

parseOp :: Int -> (Int, [Mode])
parseOp c =
  let s = show c
  in case length s of
    1 -> (read s :: Int, [P, P, P])
    2 -> (read s :: Int, [P, P, P])
    3 ->
        let
          stupid = "0" <> (take (length s - 1) s)
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          c = fromMaybe '0' $ stupid ^? element 2
          z = drop ((length s) - 1) s
        in
          ((read z :: Int), [mode a, mode b, mode c])
    4 ->
        let
          stupid = take (length s - 2) s
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          c = fromMaybe '0' $ stupid ^? element 2
          z = drop ((length s) - 2) s
        in
          ((read z :: Int), [mode a, mode b, mode c])
    5 ->
      let
        stupid = take ((length s) - 2) s
        c = fromMaybe '0' $ stupid ^? element 0
        a = fromMaybe '0' $ stupid ^? element 1
        b = fromMaybe '0' $ stupid ^? element 2
        z = drop ((length s) - 2) s
      in
        ((read z :: Int), [mode a, mode b, mode c])
    _ -> error s


type Done = Bool
type Offset = Int
type Relative = Int
type Adjust = Int
type ProgramOut = (Done, Offset, Relative, Program, IO')

                      -- v0 = case m2 of
                      --   P -> program ! (program ! (offset + 1))
                      --   I -> program ! (offset + 1)
                      --   R -> program ! (program ! ((offset + 1) + relative))


getValue :: Mode -> Program -> Offset -> Adjust -> Relative -> Int
getValue m program offset adjust relative = case m of
  R -> program ! ((program ! (offset + adjust)) + relative)
  I -> program ! (offset + adjust)
  P -> program ! (program ! (offset + adjust))

getWrite :: Mode -> Program -> Offset -> Adjust -> Relative -> Int
getWrite m program offset adjust relative = case m of
  R -> (program ! (offset + adjust)) + relative
  _ -> program ! (offset + adjust)

runProgram :: Int -> Int -> Vector Int -> ([Int], [Int]) -> ProgramOut
-- runProgram offset relative program io | trace (show $ (show $ program ! offset) <> "::" <> show relative) False = undefined
runProgram offset relative program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    store = program ! (offset + 3)
  in
    case op of
      99 -> (True, offset, relative, program, io)
      _  -> run
        where run = case op of
                -- addition
                1 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                  in
                    runProgram (offset + 4) relative (program // [(store, (+) v0 v1)]) io

                -- multiply
                2 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                  in
                    runProgram (offset + 4) relative (program // [(store, (*) v0 v1)]) io

                -- store input
                3 ->
                  let (ins, out) = io
                      (m1:m2:m3:_) = modes
                  in case length ins of
                    0 -> (False, offset, relative, program, io) -- return it for continuing to run
                    _ -> let (i:is) = ins
                         in
                          case m2 of
                            R -> runProgram (offset + 2) relative (program // [(relative + (program ! (offset + 1)), i)]) (is, out)
                            _ -> runProgram (offset + 2) relative (program // [(program ! (offset + 1), i)]) (is, out)

                -- output
                4 ->
                  let (i, o) = io
                      (m1:m2:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                  in runProgram (offset + 2) relative program (i, v0 : o)

                -- jump if true
                5 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 /= 0
                    then runProgram v1 relative program io
                    else runProgram (offset + 3) relative program io

                -- jump if false
                6 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                        -- R -> program ! (program ! ((offset + 2) + relative))
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == 0
                    then runProgram v1 relative program io
                    else runProgram (offset + 3) relative program io

                -- less than
                7 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 < v1
                    then runProgram (offset + 4) relative (program // [(store, 1)]) io
                    else runProgram (offset + 4) relative (program // [(store, 0)]) io

                -- equals
                8 ->
                  let (m1:m2:m3:_) = modes
                      v0 = getValue m2 program offset 1 relative
                      v1 = getValue m1 program offset 2 relative
                      store = getWrite m3 program offset 3 relative
                        -- R -> program ! (program ! ((offset + 2) + relative))
                      -- v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      -- v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == v1
                    then runProgram (offset + 4) relative (program // [(store, 1)]) io
                    else runProgram (offset + 4) relative (program // [(store, 0)]) io

                9 ->
                  let (m1:m2:_) = modes
                      -- v0 = program ! (program
                      v0 = getValue m2 program offset 1 relative
                  in
                    runProgram (offset + 2) (relative + v0) program io

                e -> error $ show offset <> " " <> show op <> " " <> show program

type IO' = ([Int], [Int])
type Program = Vector Int



-- 1) create field of coords + color
-- field :: [((Int, Int), Char)]
data Direction = Up | Right' | Down | Left'

turnRight Up     = Right'
turnRight Right' = Down
turnRight Down   = Left'
turnRight Left'  = Up

turnLeft Up     = Left'
turnLeft Left'  = Down
turnLeft Down   = Right'
turnLeft Right' = Up

getNewDirection 0 d = turnLeft  d
getNewDirection 1 d = turnRight d

getNewLocation x y direction =
  case direction of
    Left'  -> (x - 1, y)
    Right' -> (x + 1, y)
    Up     -> (x, y + 1)
    Down   -> (x, y - 1)

updateField x y newColor field =
  let
    colorToChar 0 = '.'
    colorToChar 1 = '#'
  in
    (element (x + (rowWidth * y)) .~ (colorToChar newColor)) field

updatePaintedField x y newColor field =
  (element (x + (rowWidth * y)) .~ '#') field


type Field = [Char]
type Robot = (Int, Int, Direction)

field = take 5000 (repeat '.')

rowWidth = 30

getColor :: Robot -> Field -> Int
getColor (x, y, _) field =
  let
    c = field !! (x + (y * rowWidth))
  in case c of
    '.' -> 0
    '#' -> 1

-- 2) robot has location and direction
---- rotate then move
runSimulation :: Robot -> Field -> Field -> ProgramOut -> Field
runSimulation robot paintedField field program =
  let
    (done, offset, relative, program', io) = program

    color = getColor robot field

    ranProgram@(done', offset', relative', program'', io') =
      runProgram offset relative program' ([color], [])

    (oldInput, (toPaint : direction : _)) = io'

    -- update robot location
    (x, y, rdirection) = robot
    newDirection = getNewDirection direction rdirection
    (newX, newY) = getNewLocation x y newDirection

    -- update field color
    newField = updateField x y toPaint field

    -- update paintedField
    paintedField' = updatePaintedField x y toPaint paintedField
  in
    case length oldInput of
      0 ->
        case done' of
          True  -> paintedField
          False -> runSimulation (newX, newY, newDirection) paintedField' newField ranProgram
      1 -> error "fuck"



partOne = do
  f <- readFile "./src/DayEleven/data.txt"
  let
    parsed = parseString numbers mempty f
    -- fixed = fmap (\x -> fromList ((take 2000 (repeat 0)) <> x <> (take 2000 (repeat 0)))) parsed
    fixed = fmap (\x -> fromList ( x <> (take 2000 (repeat 0)))) parsed

    ran = fmap (\x -> runSimulation (30, 100, Up) field field (False, 0, 0, x, ([], []))) fixed
    -- ran = fmap (\x -> runProgram 0 0 x ([], [])) fixed

  -- print ran
  print $ (fmap (\x -> length $ filter (== '#') x)  ) ran
