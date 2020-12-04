module Main where

import Text.Parsec
import Debug.Trace
import Data.Set(Set, fromList, isSubsetOf)
import System.Directory
import System.Environment(getArgs)

data PassportItem = PassportItem {
  key :: String,
  value :: String
}

digitsParser :: Int -> Parsec String () Integer
digitsParser n =
  read <$> do { res <- count n digit; eof; return res }
  -- in case parseRes of
  --   Right digits -> Just (read digits :: Int)
  --   Left _ -> Nothing

parseDigits n input =
  case parse (digitsParser n) "" input of
    Right res -> Just res
    Left _ -> Nothing

validateItem :: PassportItem -> Bool
validateItem PassportItem{ key = key, value = value } =
  case key of
    "byr" ->
      case parseDigits 4 value of
        Just num ->
          num >= 1920 && num <= 2002
        Nothing -> False
    "iyr" -> case parseDigits 4 value of
        Just num ->
          num >= 2010 && num <= 2020
        Nothing -> False
    "eyr" -> case parseDigits 4 value of
        Just num ->
          num >= 2020 && num <= 2030
        Nothing -> False
    "hgt" ->
      let parser = do{ val <- read <$> many digit
               ; metric <- string "cm" <|> string "in"
               ; return (metric, val)
               }
      in case parse parser "" value of
        Right (m, v) -> case m of
                "cm" -> v >= 150 && v <= 193
                "in" -> v >= 59 && v <= 76
        Left e -> trace (show e ++ "  input hgt: " ++ value) False
    "hcl" ->
      let parser = char '#' >> count 6 (oneOf "abcdef1234567890") >> eof
      in case parse parser "" value of
        Right _ -> True
        Left e -> trace (show e ++ "  input hcl: " ++ value) False
    "ecl" ->
      case parse (choice (map (try . string) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) >> eof) "" value of
        Right _ -> True
        Left e -> trace (show e ++ "  input ecl: " ++ value) False
    "pid" ->
      case parse (count 9 alphaNum >> eof) "" value of
        Right _ -> True
        Left e -> trace (show e ++ "  input pid: " ++ value) False
    "cid" ->
      True



instance Show PassportItem where
  show p = key p ++ ":" ++ value p

newtype Passport = Passport [PassportItem]

validate :: Set String -> Passport -> Bool
validate expectedKeys (Passport passport) =
  let fields = fromList $ map key passport
  in isSubsetOf expectedKeys fields && (length $ filter not $ map validateItem passport) == 0

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  print path
  currentDir <- getCurrentDirectory
  input <- readFile (currentDir ++ "/" ++ path)
  let parseItem = do{ key <- many1 alphaNum
                    ; char ':'
                    ; val <- manyTill anyChar (lookAhead space)
                    ; return PassportItem { key = key, value = val }
                    }
  -- let parser = do{ char '('
  --                ; val <- many (char 'a')
  --                ; char ')'
  --                ; return val
  --                }

  let parsePassport = do{ res <- endBy parseItem space
                        ; return (Passport res)
                        }
  let parsePassports = sepBy parsePassport space

  let parseResult = parse parsePassports "" input
  let result = case parseResult of
          Right passports ->
            let requiredFields = fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] --, "cid"]
            in Just (length $ filter (validate requiredFields) passports)
          Left _ -> Nothing
  print result
