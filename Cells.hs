-- all Cell related functions
module Cells where

import Parser
import Utils
import Data.Char

data CellCoords = EmptyCoords | Column (Int) | Row (Int) | Cell (Int, Int) deriving (Show, Eq)
data RangeCoords = Range (CellCoords, CellCoords) deriving (Show, Eq)

data Function = Sum | Product | Mean deriving (Show, Eq)

data CellContent = EmptyCell | NumCell (Double) | TextCell (String) | FunctionCell (Function, [RangeCoords]) deriving (Show, Eq)

printCell :: CellCoords -> IO ()
printCell EmptyCoords = putStrLn "empty"
printCell (Column x) = putStrLn ("col " ++ show x)
printCell (Cell (x, y)) = putStrLn ("cell " ++ show x ++ " " ++ show y)

getCellCoords :: String -> CellCoords
getCellCoords coords | empty /= [] = EmptyCoords
                     | otherwise = getCell(column, row) where
	[(letters, rest)] = parse (many notDigit) coords
	[(digits, empty)] = parse (many digit) rest
	column = if letters == [] then -1 else getColumnNumber letters
	row = if digits == [] then -1 else (string2int digits - 1)

getCell :: (Int, Int) -> CellCoords
getCell (-1, row) = Row row
getCell (col, -1) = Column col
getCell (col, row) = Cell (col, row)

getColumnNumber :: String -> Int
getColumnNumber letters = if legit then getColumn ords else -1 where
	ords = [ord x - ord 'A' | x <- letters]
	range = [(x <= 25) && x >= 0 | x <- ords]
	legit = allTrue range
	getColumn :: [Int] -> Int
	getColumn [] = 0
	getColumn [x] = x
	getColumn xs = (getColumn (init xs)) * 26 + last xs

getCellRange :: String -> (CellCoords, CellCoords)
getCellRange input = (case length splitCells of
	1 -> (getCellCoords (head splitCells), getCellCoords (head splitCells))
	2 -> (getCellCoords (head splitCells), getCellCoords (last splitCells))
	_ -> (EmptyCoords, EmptyCoords)) where
		splitCells = split ':' input

getCellContent :: String -> CellContent
getCellContent "" = EmptyCell
getCellContent content
  | (head content) ==  '"' && (last content) == '"' = (TextCell (drop 1 (init content)))
  | otherwise = EmptyCell


matrixReplaceAt :: [[a]] -> (CellCoords) -> a -> [[a]]
matrixReplaceAt (col:cols) (Cell (0, rowNum)) elem = ((replaceAt col rowNum elem):cols)
matrixReplaceAt (col:cols) (Cell (colNum, rowNum)) elem = (col:(matrixReplaceAt cols (Cell (colNum-1,rowNum)) elem))
matrixReplaceAt matrix _ _ = matrix