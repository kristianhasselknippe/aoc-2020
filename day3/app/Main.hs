module Main where

import Lib

import System.IO
import System.Directory
import Data.List
import Data.List.Split
import Text.Read
import System.Environment(getArgs, getProgName)

parseInput :: String -> IO String
parseInput path = do
  currentDir <- getCurrentDirectory
  readFile (currentDir ++ "/" ++ path)

hasTree :: String -> Int -> Bool
hasTree line xPos =
  let lineLen = length line
      index = xPos `mod` lineLen
  in (line !! index) == '#'

countTreesInPath :: [String] -> (Int, Int) -> Int
countTreesInPath lines (right, down) =
  let linesInPath = map snd $ filter (\(i,l) -> i `mod` down == 0) $ (zip [0..] lines)
  in sum $ map (\x -> case x of
                   True -> 1
                   False -> 0) $ zipWith hasTree linesInPath [x * right | x <- [0..]]

main :: IO ()
main = do
  argv <- getArgs
  print argv
  input <- parseInput (argv !! 0)
  print input
  let lines = (filter (\l -> length l > 0)) $ (splitOn "\n" input)
  let resultP1 = countTreesInPath lines (3, 1)
  print ("Result p1: " ++ (show resultP1))
  let paths = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  let resultP2 = product $ map (countTreesInPath lines) paths
  print ("result p2: " ++ (show resultP2))
