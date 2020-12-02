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
  range :: (Int, Int),
  letter :: Char,
  value :: String
}
instance Show Policy where
  show p = "Policy: " ++ (show (range p)) ++ " " ++ (show (letter p)) ++ " " ++ (show (value p))

parseLine :: String -> Policy
parseLine line =
  let parts = splitOn " " line
      range = let [a,b] = (map read (splitOn "-" (parts !! 0)) :: [Int])
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

safeIndex :: [a] -> Int -> Maybe a
safeIndex l i
       | i >= 0 && i < length l = Just (l !! i)
       | otherwise = Nothing

validatePart2 :: Policy -> Bool
validatePart2 p =
  let
      theLetter = letter p
      (first, second) = range p
      firstLetter = safeIndex (value p) (first-1)
      secondLetter = safeIndex (value p) (second-1)
  in case (firstLetter, secondLetter) of
          (Just f, Just s) -> ((f == theLetter) || (s == theLetter)) && f /= s
          (Just f, Nothing) -> f == theLetter
          (Nothing, Just s) -> s == theLetter
          (Nothing, Nothing) -> False

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  print currentDir
  fileContent <- readFile (currentDir ++ "/../input-sets/day2-1.txt")
  --print fileContent
  let lines  = filter (\x -> (length x) > 0) (splitOn "\n" fileContent)
      policies = map parseLine lines
      validated = filter id (map validatePart2 policies)
      numValidated = length validated
  print ("Number of valid: " ++ (show numValidated))
