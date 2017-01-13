-- functions that execute user's commands
module Commands where

import Help
import Cells
import Utils

tryExecuteCommand :: [[CellContent]] -> String -> IO [[CellContent]]
tryExecuteCommand sheet fullCommand = newSheet where
	newSheet = (case command of
		"show" -> do
			tryDisplay sheet args
			return sheet
		"help" -> do
			printHelpMsg [args]
			return sheet
		"set" -> trySetValue sheet args
		"add" -> tryAddCells sheet args
		"delete" -> tryDeleteCells sheet args
		_ -> do
			putStrLn ("There is no " ++ command ++ " command.")
			return sheet)
	(command, args) = splitOnce ' ' fullCommand


tryDisplay :: [[CellContent]] -> String -> IO ()
tryDisplay sheet [] = display sheet (EmptyCoords, EmptyCoords)
tryDisplay sheet coords = display sheet (getCellRange coords)


display :: [[CellContent]] -> (CellCoords, CellCoords) -> IO ()
display _ (EmptyCoords, EmptyCoords) = putStrLn "Showing full sheet"
display sheet (Cell (x1, y1), Cell (x2, y2)) = if x1 > x2 || y1 > y2 then
 	putStrLn "Incorrect show arguments."
	else if x1 == x2 && y1 == y2 then putStrLn(show ((sheet !! x1) !! y1)) else putStrLn("Showing range")
display _ (Column col1, Column col2) = if col1 > col2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show col1 ++ ":" ++ show col2)
display _ (Row row1, Row row2) = if row1 > row2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show row1 ++ ":" ++ show row1)
display _ _ = putStrLn "Incorrect show arguments."


trySetValue :: [[CellContent]] -> String -> IO [[CellContent]]
trySetValue sheet args = do
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


tryAddCells :: [[CellContent]] -> String -> IO [[CellContent]]
tryAddCells sheet input = do
	putStrLn message
	return newSheet where
		coords = getCellCoords input
		ok = verifyRowOrColumn coords
		newSheet = if ok
			then addCells sheet coords
			else sheet
		message = if ok
			then "Inserted correctly."
			else "Incorrect add command."

addCells :: [[CellContent]] -> CellCoords -> [[CellContent]]
addCells [] (Column col) = [[EmptyCell | x <- [0..9]] | y <- [0..col]]
addCells [] (Row row) = [[EmptyCell | x <- [0..row]] | y <- [0..9]]
addCells sheet (Column col) = (insertAt sheet col column) where
	rows = length (head sheet)
	column = [EmptyCell | x <- [1..rows]]	
addCells sheet (Row row) =
	[insertAt (sheet !! i) row EmptyCell | i <- [0..((length sheet) - 1)]]


tryDeleteCells :: [[CellContent]] -> String -> IO [[CellContent]]
tryDeleteCells [] _ = do
	putStrLn "The sheet is empty, nothing to remove."
	return []
tryDeleteCells sheet input = (case coords of
	(Column col) -> if col >= colCount 
		then do
			putStrLn "Column doesn't exist."
			return sheet
		else do
			putStrLn "Column removed successfully."
			return (deleteCells sheet coords)
	(Row row) -> if row >= rowCount
		then do
			putStrLn "Row doesn't exist."
			return sheet
		else do
			putStrLn "Row removed successfully."
			return (deleteCells sheet coords)) where
				coords = getCellCoords input
				colCount = length sheet
				rowCount = length (head sheet)

deleteCells :: [[CellContent]] -> CellCoords -> [[CellContent]]
deleteCells sheet (Column col) = removeAt sheet col
deleteCells sheet (Row row) = if head newSheet == []
	then [] 
	else newSheet where 
		newSheet = [removeAt (sheet !! col) row | col <- [0..(colCount-1)]]
		colCount = length sheet