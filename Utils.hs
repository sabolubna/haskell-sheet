-- miscelleanous functions
module Utils where

import Data.Char
import Parser

split :: Char -> String -> [String]
split _ [] = []
split separator input = [word] ++ split separator (drop 1 rest) where
  (word, rest) = head (parse (many (notChar separator)) input)

splitOnce :: Char -> String -> (String, String)
splitOnce _ [] = ([], [])
splitOnce separator input = (word, drop 1 rest) where
	(word, rest) = head (parse (many (notChar separator)) input)

notChar :: Char -> Parser Char
notChar x = sat(/= x)

nonSpace :: Parser Char
nonSpace = sat (/= ' ')

notDigit :: Parser Char
notDigit = sat (isNotDigit)

isNotDigit :: Char -> Bool
isNotDigit c = not (isDigit c)

string2int :: String -> Int
string2int [] = 0
string2int [x] = digitToInt x
string2int xs = digitToInt(last(xs)) + string2int(init xs) * 10

allTrue :: [Bool] -> Bool
allTrue xs = foldr (&&) True xs