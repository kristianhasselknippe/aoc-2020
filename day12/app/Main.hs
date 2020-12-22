module Main where


import System.Environment (getArgs)
import System.Directory
import Data.List.Split
import Flow
import Debug.Trace

type Position = (Int, Int)

data Movement = North | South | East | West | Forward | Left | Right

type Direction = Int

type State = (Direction, Position)

type Action = (Movement, Int)

moveShip :: State -> Action -> State
moveShip (dir, (x,y)) (move, amount) =
    case move of
      North -> (dir, (x, y - amount))
      South -> (dir, (x, y + amount))
      East -> (dir, (x + amount, y))
      West -> (dir, (x - amount, y))
      Forward -> case (trace ("Dir: " ++ show dir) dir) of
          0 -> moveShip (dir, (x,y)) (East, amount)
          90 -> moveShip (dir, (x,y)) (South, amount)
          180 -> moveShip (dir, (x,y)) (West, amount)
          270 -> moveShip (dir, (x,y)) (North, amount)
      Main.Left -> ((dir - amount) `mod` 360, (x,y))
      Main.Right -> ((dir + amount) `mod` 360, (x,y))

decodeAction :: String -> Action
decodeAction actionString =
        let (actionC:numStr) = actionString
            action = case (trace ("Action: " ++ show actionC) actionC) of
              'N' -> North
              'S' -> South
              'W' -> West
              'E' -> East
              'L' -> Main.Left
              'F' -> Forward
              'R' -> Main.Right
            num = read numStr :: Int
        in (action, num)

part1 input =
  let actions = input |> map decodeAction
  in actions |> foldl moveShip (0, (0,0))


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let input = content |> splitOn "\n" |> filter (not . null)
  print input
  let (dir, (x,y)) = part1 input
  let dist = abs x + abs y
  print ("Output part1: " ++ show dist)
