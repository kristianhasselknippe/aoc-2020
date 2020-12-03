module Main where

import Lib

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import Debug.Trace
import Text.Read
import System.Environment(getArgs, getProgName)

parseInput :: String -> IO String
parseInput path = do
  currentDir <- getCurrentDirectory
  readFile (currentDir ++ "/" ++ path)

hasTree :: String -> Int -> Bool
hasTree line xPos =
  let lineLen = length line
      index = (trace ("XPos: " ++ show xPos) xPos) `mod` (trace ("Line len: " ++ show lineLen) lineLen)
  in (line !! (traceShow index index)) == '#'

countTreesInPath :: [String] -> Int
countTreesInPath lines =
  sum $ map (\x -> case x of
                   True -> 1
                   False -> 0) $ zipWith hasTree lines [x * 3 | x <- [0..]]

main :: IO ()
main = do
  argv <- getArgs
  print argv
  input <- parseInput (argv !! 0)
  print input
  let result = countTreesInPath $ (filter (\l -> length l > 0)) $ (splitOn "\n" input)
  print ("Result: " ++ (show result))
