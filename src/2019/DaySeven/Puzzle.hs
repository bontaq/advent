module DaySeven.Puzzle where

import Text.Parser.Combinators (many, try, optional)
import Text.Parser.Token (token, integer)
import Text.Parser.Char (char)
import Text.Trifecta.Parser (Parser, parseString)
import Text.Trifecta.Result
import Data.Vector (fromList, toList, (!), (//), Vector)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (sortBy)
import Control.Lens
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Debug.Trace

number :: Parser Int
number = do
  i <- integer
  _ <- optional (char ',')
  pure (fromIntegral i)

numbers :: Parser [Int]
numbers = many number

data Mode = I | P
            deriving (Show, Eq)

mode '1' = I
mode '0' = P

parseOp :: Int -> (Int, [Mode])
parseOp c =
  let s = show c
  in case length s of
    1 -> (read s :: Int, [P, P])
    2 -> (read s :: Int, [P, P])
    3 ->
        let
          stupid = "0" <> (take (length s - 1) s)
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          z = drop ((length s) - 1) s
        in
          ((read z :: Int), [mode a, mode b])
    4 ->
        let
          stupid = take (length s - 2) s
          a = fromMaybe '0' $ stupid ^? element 0
          b = fromMaybe '0' $ stupid ^? element 1
          z = drop ((length s) - 2) s
        in
          ((read z :: Int), [mode a, mode b])
    _ -> error s


type Done = Bool
type Offset = Int
type ProgramOut = (Done, Offset, Program, IO')

runProgram :: Int -> Vector Int -> ([Int], [Int]) -> ProgramOut
-- runProgram offset program io | trace (show  " " <> show io) False = undefined
runProgram offset program io =
  let
    opcode = program ! offset
    (op, modes) = parseOp opcode

    store = program ! (offset + 3)
  in
    case op of
      99 -> (True, offset, program, io)
      _  -> run
        where run = case op of
                -- addition
                1 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (+) v0 v1)]) io

                -- multiply
                2 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    runProgram (offset + 4) (program // [(store, (*) v0 v1)]) io

                -- store input
                3 ->
                  let (ins, out) = io
                  in case length ins of
                    0 -> (False, offset, program, io) -- return it for continuing to run
                    _ -> let (i:is) = ins
                         in
                           runProgram (offset + 2) (program // [(program ! (offset + 1), i)]) (is, out)

                -- output
                4 ->
                  let (i, o) = io
                      (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                  in runProgram (offset + 2) program (i, v0 : o)

                -- jump if true
                5 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 /= 0
                    then runProgram v1 program io
                    else runProgram (offset + 3) program io

                -- jump if false
                6 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == 0
                    then runProgram v1 program io
                    else runProgram (offset + 3) program io

                -- less than
                7 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 < v1
                    then runProgram (offset + 4) (program // [(store, 1)]) io
                    else runProgram (offset + 4) (program // [(store, 0)]) io

                -- equals
                8 ->
                  let (m1:m2:_) = modes
                      v0 = if m2 == I then program ! (offset + 1) else program ! (program ! (offset + 1))
                      v1 = if m1 == I then program ! (offset + 2) else program ! (program ! (offset + 2))
                  in
                    if v0 == v1
                    then runProgram (offset + 4) (program // [(store, 1)]) io
                    else runProgram (offset + 4) (program // [(store, 0)]) io

                e -> error $ show offset <> " " -- <> show program

type IO' = ([Int], [Int])
type Program = Vector Int
type ThrusterOut = [Int]

runVariation :: Program -> (Int, Int, Int, Int, Int) -> ([Int], ThrusterOut)
runVariation program (a,b,c,d,e) =
  let
    (_, _, _, (_, runAOut)) = runProgram 0 program ([a, 0], [])
    (_, _, _, (_, runBOut)) = runProgram 0 program ([b, head runAOut], [])
    (_, _, _, (_, runCOut)) = runProgram 0 program ([c, head runBOut], [])
    (_, _, _, (_, runDOut)) = runProgram 0 program ([d, head runCOut], [])
    (_, _, _, (_, runEOut)) = runProgram 0 program ([e, head runDOut], [])
  in
    ([a,b,c,d,e], runEOut)

filterUniq :: [(Int, Int, Int, Int, Int)] -> [(Int, Int, Int, Int, Int)]
filterUniq = filter (not . dupe)
  where
    dupe (a,b,c,d,e) =
       let
         n x = filter (\y -> y == x) [a,b,c,d,e]
       in
         any (\x -> x > 1) $ fmap (length . n) [a,b,c,d,e]

runVariations :: Program -> [([Int], ThrusterOut)]
runVariations program =
  let
    variations = [(a,b,c,d,e) | a <- [0..4], b <- [0..4], c <- [0..4], d <- [0..4], e <- [0..4]]
    filtered = filterUniq variations
  in
    fmap (runVariation program) filtered

partOne = do
  f <- readFile "./src/DaySeven/data.txt"
  let
    program = parseString numbers mempty f
    variations = fmap (runVariations . fromList) program

  print $ fmap (sortBy (\(_, a) (_, b) -> compare b a)) variations

runComputer :: TChan Int -> TChan Int -> Int -> ProgramOut -> IO ()
runComputer hout hin phase (done, offset, program, io) = do
  let (input', output') = io

  realInput <- if length input' == 0 && done == False
               then atomically $ fmap (\x -> [x]) $ readTChan hin
               else if length input' == 0
                       then pure []
                       else pure input'

  let newProgram@(done', offset', program', (i, o)) =
        runProgram offset program (realInput, [])

  case length o of
    0 -> do -- runComputer hout hin phase newProgram
      if not done'
      then runComputer hout hin phase newProgram
      else pure ()
    1 -> do
      atomically $ writeTChan hout (head o)
      runComputer hout hin phase newProgram
    _ -> error "fuck"

  -- runComputer hout hin phase newProgram

runLastComputer :: MVar (IO') -> TChan Int -> TChan Int -> Int -> ProgramOut -> IO ()
runLastComputer fin hout hin phase (done, offset, program, io) = do
  let (input', output') = io

  realInput <- if length input' == 0 && done == False
               then atomically $ fmap (\x -> [x]) $ readTChan hin
               else if length input' == 0
                       then pure []
                       else pure input'

  let newProgram@(done', offset', program', (i, o)) =
        runProgram offset program (realInput, [])

  case length o of
    0 -> do -- runComputer hout hin phase newProgram
      if not done'
      then runLastComputer fin hout hin phase newProgram
      else putMVar fin io
    1 -> do
      atomically $ writeTChan hout (head o)
      runLastComputer fin hout hin phase newProgram
    _ -> error "fuck"

runVariations' :: Program -> (Int, Int, Int, Int, Int) -> IO (IO')
runVariations' program (a', b', c', d', e') = do
  a <- atomically newTChan
  b <- atomically newTChan
  c <- atomically newTChan
  d <- atomically newTChan
  e <- atomically newTChan

  finished <- newEmptyMVar

  forkIO $ runComputer b a 0 (False, 0, program, ([a', 0], []))
  forkIO $ runComputer c b 0 (False, 0, program, ([b'], []))
  forkIO $ runComputer d c 0 (False, 0, program, ([c'], []))
  forkIO $ runComputer e d 0 (False, 0, program, ([d'], []))

  forkIO $ runLastComputer finished a e 0 (False, 0, program, ([e'], []))

  pout <- takeMVar finished
  pure pout

fromResult (Success a) = a

partTwo = do
  f <- readFile "./src/DaySeven/data.txt"
  let
    variations = [(a,b,c,d,e) | a <- [5..9], b <- [5..9], c <- [5..9], d <- [5..9], e <- [5..9]]
    filtered = filterUniq variations
    program = fromResult $ parseString numbers mempty f

  variations <- mapM (runVariations' (fromList program)) filtered
  -- print variations

  -- v <- variations

  print $ take 10 $ (sortBy (\(_, (a:_)) (_, (b:_)) -> compare b a)) variations

  pure ()
