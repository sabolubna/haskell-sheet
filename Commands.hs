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
		"delete" -> deleteCells sheet args
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
		ok = verifyCell cellCoords && cellContent /= EmptyCell
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
		ok = verifyRowOrColumn coords
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


deleteCells :: [[CellContent]] -> String -> IO [[CellContent]]
deleteCells [] _ = do
	putStrLn "The sheet is empty, nothing to remove."
	return []
deleteCells sheet input = (case coords of
	(Column col) -> if col >= colCount 
		then do
			putStrLn "Column doesn't exist."
			return sheet
		else do
			putStrLn "Column removed successfully."
			return (removeCells sheet coords)
	(Row row) -> if row >= rowCount
		then do
			putStrLn "Row doesn't exist."
			return sheet
		else do
			putStrLn "Row removed successfully."
			return (removeCells sheet coords)) where
				coords = getCellCoords input
				colCount = length sheet
				rowCount = length (head sheet)

removeCells :: [[CellContent]] -> CellCoords -> [[CellContent]]
removeCells sheet (Column col) = removeAt sheet col
removeCells sheet (Row row) = if head newSheet == []
	then [] 
	else newSheet where 
		newSheet = [removeAt (sheet !! col) row | col <- [0..(colCount-1)]]
		colCount = length sheet