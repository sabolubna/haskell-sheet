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
		notOk = cellCoords == EmptyCoords || cellContent == EmptyCell
		newSheet = if notOk
			then sheet
			else matrixReplaceAt sheet cellCoords cellContent
		message = if notOk 
			then "Incorrect set command."
			else "OK."