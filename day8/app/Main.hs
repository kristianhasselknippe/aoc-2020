module Main where

import System.Directory
import System.Environment
import Text.Parsec
import Flow
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec.Prim(Parser)
import Data.List.Split
import qualified Data.Set as Set
import Debug.Trace

decodeIns input =
   let [op, arg] = splitOn " " input
   in (op, arg)

parseArgument :: String -> Int
parseArgument (m:rest) =
  case m of
    '+' -> read rest
    '-' -> read rest |> negate

type Op = (String, String)
type Program = [Op]
type MachineState = (Int, Int)

data OpResult = RetriedInstruction MachineState
              | Complete MachineState

instance Show OpResult where
  show (RetriedInstruction ms) = show ms
  show (Complete ms) = show ms

applyOp :: Program -> Set.Set Int -> MachineState -> OpResult
applyOp program previouslyRunOps (instructionCounter, accumulator) =
  let (code, arg) = program !! instructionCounter
      offset = parseArgument arg
      nextState = case code of
        "acc" -> (instructionCounter + 1, accumulator + offset)
        "jmp" -> (instructionCounter + offset, accumulator)
        "nop" -> (instructionCounter + 1, accumulator)
      (nextOp, _) = nextState
  in if nextOp `Set.member` previouslyRunOps
      then RetriedInstruction (instructionCounter, accumulator)
      else
          if nextOp >= length program
            then Complete nextState
            else applyOp program (Set.insert instructionCounter previouslyRunOps) nextState

swapOp (op, arg) =
  case op of
    "jmp" -> ("nop", arg)
    "nop" -> ("jmp", arg)
    _ -> (op, arg)

findWorkingSwap :: [Program] -> OpResult
findWorkingSwap (testCase:restOfTests) =
  let res = applyOp testCase Set.empty (0, 0)
  in case res of
    RetriedInstruction _ -> findWorkingSwap restOfTests
    Complete x -> Complete x
findWorkingSwap [] = RetriedInstruction (-1, -1)

main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let program = splitOn "\n" content |> filter (not . null) |> map decodeIns
  let res = applyOp program Set.empty (0, 0)
  case res of
    RetriedInstruction (ic, acc) -> print $ "Result 1: instruction counter: " ++ show ic ++ ", accumulator: " ++ show acc
    _ -> print "Error part 1"
  let programWithIndex = zip program [0..]
  let testCases = [0..length program - 1] |> map (\i -> map (\(o, index) -> if i == index then swapOp o else o) programWithIndex)
  let (Complete (ic, acc)) = findWorkingSwap testCases
  print $ "Result part 2: instruction counter: " ++ show ic ++ ", accumulator: " ++ show acc
