module Main where

import Lib
import System.IO
import System.Directory
import Data.List
import Data.List.Split
import Debug.Trace
import Text.Read

p1 :: IO ()
p1 = do
  currentDir <- getCurrentDirectory
  print currentDir
  fileContent <- readFile (currentDir ++ "/../input-sets/day1-1.txt")
  print fileContent
  let lines  = filter (\x -> (length x) > 0) (splitOn "\n" fileContent)
  print "LINES"
  print lines
  let numbers = map read lines :: [Float]
  print "NUMBERS: "
  print numbers
  let perms = [(x,y) | (x:rest) <- tails numbers, y <- rest]
  print perms
  let valid = filter (\(a,b) -> a + b == 2020) perms
  print "\n\n\n VALID: "
  let result =  (map (\(x, y) -> x * y) valid)
  print "\n RESULT: "
  print result

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  print currentDir
  fileContent <- readFile (currentDir ++ "/../input-sets/day1-1.txt")
  print fileContent
  let lines  = filter (\x -> (length x) > 0) (splitOn "\n" fileContent)
  print "LINES"
  print lines
  let numbers = map read lines :: [Int]
  print "NUMBERS: "
  print numbers
  let perms = [(x,y,z) | (x:rest) <- tails numbers, (y:yrest) <- tails rest, z <- yrest]
  let valid = filter (\(a,b,c) -> a + b + c == 2020) perms
  let result =  (map (\(x, y, z) -> x * y * z) valid)
  print "\n RESULT: "
  print result
