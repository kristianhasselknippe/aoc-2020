module Main where


import System.Environment (getArgs)
import System.Directory
import Data.List.Split
import Flow
import Debug.Trace

type Position = (Int, Int)

data Movement = North | South | East | West | Forward | Left | Right

type Direction = Int

type State = (Position, Position)

type Action = (Movement, Int)

rotatePoint :: Position -> Int -> Position
rotatePoint (xx,yy) amount =
    let x = fromIntegral xx
        y = fromIntegral yy
        rads = fromIntegral amount * (pi / 180.0)
        newX = round (x * cos rads - y * sin rads)
        newY = round (y * cos rads + x * sin rads)
    in trace ("New WP: " ++ show (newX, newY)) (newX, newY)

forward ((x,y), (wpX, wpY)) amount =
    ((x + (wpX * amount), y + wpY * amount), (wpX, wpY))

moveShip :: State -> Action -> State
moveShip (pos, (wpX,wpY)) (move, amount) =
   let ret =
        case move of
          North -> (pos, (wpX, wpY + amount))
          South -> (pos, (wpX, wpY - amount))
          East -> (pos, (wpX + amount, wpY))
          West -> (pos, (wpX - amount, wpY))
          Forward -> forward (pos, (wpX, wpY)) amount
          Main.Left -> (pos, rotatePoint (wpX, wpY)  amount)
          Main.Right -> (pos, rotatePoint (wpX, wpY) (negate amount))
   in trace ("New pos: " ++ show ret) ret

decodeAction :: String -> Action
decodeAction actionString =
        let (actionC:numStr) = actionString
            action = case (trace ("Action: " ++ show actionC ++ ", AM: " ++ show numStr) actionC) of
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
  in actions |> foldl moveShip ((0,0), (10,1))


main :: IO ()
main = do
  argv <- getArgs
  let path = head argv
  currentDir <- getCurrentDirectory
  content <- readFile $ currentDir ++ "/" ++ path
  let input = content |> splitOn "\n" |> filter (not . null)
  print input
  let ((x,y), wp) = part1 input
  let dist = abs x + abs y
  print ("Output part1: " ++ show dist)
