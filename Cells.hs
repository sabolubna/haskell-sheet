-- all Cell related functions
module Cells where

import Parser
import Utils
import Data.Char

data CellCoords = EmptyCoords | Column (Int) | Row (Int) | Cell (Int, Int) deriving Show

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