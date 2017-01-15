-- all Cell related functions
{-# LANGUAGE DeriveGeneric #-}

module Cells where

import Parser
import Utils
import Data.Char
import Control.Exception 
import Data.Binary
import GHC.Generics (Generic)

data CellCoords = EmptyCoords | Column (Int) | Row (Int) | Cell (Int, Int) deriving (Show, Eq)
data RangeCoords = Range (CellCoords, CellCoords) deriving (Show, Eq)

data Function = Sum | Product | Mean deriving (Show, Eq)

data CellContent = EmptyCell | NumCell (Double) | TextCell (String) | FunctionCell (Function, RangeCoords) deriving (Show, Eq, Generic)

-- Given a string, e.g. "F5" returns a corresponding CellCoords;
-- can be coordinates of a cell or only a column/row
getCellCoords :: String -> CellCoords
getCellCoords coords | empty /= [] = EmptyCoords
                     | otherwise = getCell(column, row) where
	[(letters, rest)] = parse (many notDigit) coords
	[(digits, empty)] = parse (many digit) rest
	column = if letters == [] then -1 else getColumnNumber letters
	row = if digits == [] then -1 else (string2int digits - 1)

-- Given coordinates of a cell/column/row, returns corresponding CellCoords
getCell :: (Int, Int) -> CellCoords
getCell (-1, row) = Row row
getCell (col, -1) = Column col
getCell (col, row) = Cell (col, row)

-- Given a column name, returns corresponding number, e.g. "C" -> 2
getColumnNumber :: String -> Int
getColumnNumber letters = if legit then getColumn ords else -1 where
	ords = [ord x - ord 'A' | x <- letters]
	range = [(x <= 25) && x >= 0 | x <- ords]
	legit = allTrue range
	getColumn :: [Int] -> Int
	getColumn [] = 0
	getColumn [x] = x
	getColumn xs = ((getColumn (init xs)) + 1) * 26  + last xs

-- Given CellCoords, return corresponding name, e.g. Column 2 -> "C"
getCoordsString :: CellCoords -> String
getCoordsString (Row row) = show (row + 1)
getCoordsString (Column col) = [chr (ord 'A' + (mod col 26))]
getCoordsString (Cell (col, row)) = getCoordsString (Column col) ++ getCoordsString (Row row)

-- Given a cell range in a string, returns CellCoords pair;
-- e.g. "A5:C7" -> (Cell (0,4), Cell (3, 6))
getCellRange :: String -> (CellCoords, CellCoords)
getCellRange input = (case length splitCells of
	1 -> (getCellCoords (head splitCells), getCellCoords (head splitCells))
	2 -> (getCellCoords (head splitCells), getCellCoords (last splitCells))
	_ -> (EmptyCoords, EmptyCoords)) where
		splitCells = split ':' input

-- Given a string from user, parses it and returns CellContent
getCellContent :: String -> CellContent
getCellContent "" = EmptyCell
getCellContent content
  | (head content) ==  '"' && (last content) == '"' = (TextCell (drop 1 (init content)))
  | (reads content :: [(Double, String)]) /= [] && (snd (head (reads content :: [(Double, String)]))) == "" = NumCell (fst (head (reads content :: [(Double, String)])))
  | verifySumExpression content = FunctionCell(Sum, Range(getCellRange (drop 4 (init content))))
  | verifyMeanExpression content = FunctionCell(Mean, Range(getCellRange (drop 5 (init content))))
  | verifyProductExpression content = FunctionCell(Product, Range(getCellRange (drop 8 (init content))))
  | otherwise = EmptyCell

-- Given a list of lists, CellCoords and new element, puts it in the 
-- corresponding "cell", and returns modified list of lists
matrixReplaceAt :: [[a]] -> (CellCoords) -> a -> [[a]]
matrixReplaceAt (col:cols) (Cell (0, rowNum)) elem = ((replaceAt col rowNum elem):cols)
matrixReplaceAt (col:cols) (Cell (colNum, rowNum)) elem = (col:(matrixReplaceAt cols (Cell (colNum-1,rowNum)) elem))
matrixReplaceAt matrix _ _ = matrix

-- Checks whether given CellCoords is a Cell
verifyCell :: CellCoords -> Bool
verifyCell (Cell (_, _)) = True
verifyCell _ = False

-- Checks whether given CellCoords is a Row or Column
verifyRowOrColumn :: CellCoords -> Bool
verifyRowOrColumn (Column _) = True
verifyRowOrColumn (Row _) = True
verifyRowOrColumn _ = False

-- Checks if a String is expression of sum.
verifySumExpression :: String -> Bool
verifySumExpression content = (take 4 content) == "SUM("  && (last content) == ')' -- needs more checks - easy to crash app.

-- Checks if a String is expression of mean.
verifyMeanExpression :: String -> Bool
verifyMeanExpression content = (take 5 content) == "MEAN("  && (last content) == ')' --  needs more checks - easy to crash app.

-- Checks if a String is expression of sum.
verifyProductExpression :: String -> Bool
verifyProductExpression content = (take 8 content) == "PRODUCT("  && (last content) == ')' --  needs more checks - easy to crash app.

-- Given a FunctionCell returns a corresponding string
functionToString :: CellContent -> String
functionToString (FunctionCell (f, Range (coord1, coord2))) =
	show f ++ "(" ++ getCoordsString coord1 ++ ":" ++ getCoordsString coord2 ++ ")"
	
