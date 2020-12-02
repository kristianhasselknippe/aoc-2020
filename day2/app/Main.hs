module Main where

import Lib
import System.IO
import System.Directory
import Data.List
import Data.List.Split
import Debug.Trace
import Text.Read

-- instance Read (Integer, Integer) where
--   read (a,b) = "(" ++ a ++ ", " ++ b ++ ")"

data Policy = Policy {
  range :: (Integer, Integer),
  letter :: Char,
  value :: String
}
instance Show Policy where
  show p = "Policy: " ++ (show (range p)) ++ " " ++ (show (letter p)) ++ " " ++ (show (value p))

parseLine :: String -> Policy
parseLine line =
  let parts = splitOn " " line
      range = let [a,b] = (map read (splitOn "-" (parts !! 0)) :: [Integer])
              in (a,b)
      letter = parts !! 1 !! 0 :: Char
      value = parts !! 2
  in Policy {
    range = range,
    letter = letter,
    value = value
  }

validate :: Policy -> Bool
validate p =
  let (low, high) = range p
      counted = foldl
              (\ a b -> (case b == (letter p) of
                                    True -> a + 1
                                    False -> a))
              0
              (value p)
  in low <= counted && counted <= high

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  print currentDir
  fileContent <- readFile (currentDir ++ "/../input-sets/day2-1.txt")
  print fileContent
  let lines  = filter (\x -> (length x) > 0) (splitOn "\n" fileContent)
      policies = map parseLine lines
      validated = filter (\x -> x == True) (map validate policies)
      numValidated = length validated
  print ("Number of valid: " ++ (show numValidated))
