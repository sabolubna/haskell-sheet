-- functions that execute user's commands
module Commands where

import Help
import Cells
import Utils

tryExecuteCommand :: [[CellContent]] -> String -> IO [[CellContent]]
tryExecuteCommand sheet fullCommand = newSheet where
	newSheet = (case command of
		"show" -> do
			display sheet args
			return sheet
		"help" -> do
			printHelpMsg [args]
			return sheet
		"set" -> setValue sheet args
		"add" -> addCells sheet args
		_ -> do
			putStrLn ("There is no " ++ command ++ " command.")
			return sheet)
	(command, args) = splitOnce ' ' fullCommand


display :: [[CellContent]] -> String -> IO ()
display sheet [] = printSheet sheet (EmptyCoords, EmptyCoords)
display sheet coords = printSheet sheet (getCellRange coords)


printSheet :: [[CellContent]] -> (CellCoords, CellCoords) -> IO ()
printSheet _ (EmptyCoords, EmptyCoords) = putStrLn "Showing full sheet"
printSheet sheet (Cell (x1, y1), Cell (x2, y2)) = if x1 > x2 || y1 > y2 then
 	putStrLn "Incorrect show arguments."
	else if x1 == x2 && y1 == y2 then putStrLn(show ((sheet !! x1) !! y1)) else putStrLn("Showing range")
printSheet _ (Column col1, Column col2) = if col1 > col2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show col1 ++ ":" ++ show col2)
printSheet _ (Row row1, Row row2) = if row1 > row2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show row1 ++ ":" ++ show row1)
printSheet _ _ = putStrLn "Incorrect show arguments."


setValue :: [[CellContent]] -> String -> IO [[CellContent]]
setValue sheet args = do
	putStrLn message
	return newSheet where
		(coords, value) = splitOnce ' ' args
		cellCoords = getCellCoords coords
		cellContent = getCellContent value
		ok = cellCoords /= EmptyCoords && cellContent /= EmptyCell
		newSheet = if ok
			then matrixReplaceAt sheet cellCoords cellContent
			else sheet
		message = if ok 
			then "OK."
			else "Incorrect set command."

addCells :: [[CellContent]] -> String -> IO [[CellContent]]
addCells sheet input = do
	putStrLn message
	return newSheet where
		coords = getCellCoords input
		ok = case coords of
			(Column col) -> True
			(Row row) -> True
			_ -> False
		newSheet = if ok
			then insertEmpty sheet coords
			else sheet
		message = if ok
			then "Inserted correctly."
			else "Incorrect add command."

insertEmpty :: [[CellContent]] -> CellCoords -> [[CellContent]]
insertEmpty [] (Column col) = [[EmptyCell | x <- [0..9]] | y <- [0..col]]
insertEmpty [] (Row row) = [[EmptyCell | x <- [0..row]] | y <- [0..9]]
insertEmpty sheet (Column col) = (insertAt sheet col column) where
	rows = length (head sheet)
	column = [EmptyCell | x <- [1..rows]]	
insertEmpty sheet (Row row) =
	[insertAt (sheet !! i) row EmptyCell | i <- [0..((length sheet) - 1)]]