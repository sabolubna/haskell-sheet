-- functions that execute user's commands
module Commands where

import Help
import Cells
import Utils

columnWidth = 10
totalWidth = 70

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

-- show command
tryDisplay :: [[CellContent]] -> String -> IO ()
tryDisplay sheet [] = do
	putStrLn (getFirstRow ((Column 0), (Column (colCount-1))))
	display sheet (Cell (0, 0), Cell (colCount-1, rowCount-1)) where
		colCount = length sheet
		rowCount = length (head sheet)
tryDisplay sheet input = (case coords of
	(Cell (x1, y1), Cell (x2, y2)) -> if x1 > x2 || y1 > y2 || x2 >= colCount || y2 >= rowCount
		then putStrLn "Incorrect show arguments."
		else if x1 == x2 && y1 == y2 
			then displayFullElem sheet (Cell (x1, y1))
			else do
				putStrLn (getFirstRow ((Column x1), (Column x2)))
				display sheet (Cell (x1, y1), Cell (x2, y2))
	(Column col1, Column col2) -> if col1 > col2 || col2 >= colCount
		then putStrLn "Incorrect show arguments."
		else do
				putStrLn (getFirstRow ((Column col1), (Column col2)))
				display sheet (Column col1, Column col2)		
	(Row row1, Row row2) -> if row1 > row2 || row2 >= rowCount
		then putStrLn "Incorrect show arguments."
		else do
				putStrLn (getFirstRow ((Column 0), (Column (colCount-1))))
				display sheet (Row row1, Row row2)) where 
			coords = getCellRange input
			colCount = length sheet
			rowCount = if colCount > 0 then length (head sheet) else 0

display :: [[CellContent]] -> (CellCoords, CellCoords) -> IO ()
display [] (_, _) = putStrLn "Sheet is empty."
display sheet (Cell (x1, y1), Cell (x2, y2)) = if y1 == y2
	then putStrLn (getRow sheet (Row y1) (Column x1) (Column x2))
	else do
		putStrLn (getRow sheet (Row y1) (Column x1) (Column x2))
		display sheet (Cell (x1, y1+1), Cell (x2, y2))
display sheet (Column col1, Column col2) = 
	display sheet (Cell (col1, 0), Cell (col2, length (head sheet) - 1))
display sheet (Row row1, Row row2) = 
	display sheet (Cell (0, row1), Cell (length sheet - 1, row2))
display _ _ = putStrLn "Incorrect show arguments."

getFirstRow :: (CellCoords, CellCoords) -> String
getFirstRow ((Column col1), (Column col2)) = rowString where
	elems = [[getCoordsString (Column col)] ++ [" " | x <- [1..(columnWidth - length (getCoordsString (Column col)))]] ++ [" | "]| col <- [col1..col2]]
	rowString = "  | " ++ (take totalWidth (foldr (++) "" (foldr (++) [""] elems)))

getRow :: [[CellContent]] -> CellCoords -> CellCoords -> CellCoords -> String
getRow sheet (Row row) (Column x1) (Column x2) = rowString where
	elems = [getElemPart columnWidth ((sheet !! col) !! row) ++ " | " | col <- [x1..x2]]
	rowString = show (row+1) ++ " | " ++ (take totalWidth (foldr (++) "" elems))

getElemPart :: Int -> CellContent -> String
getElemPart n EmptyCell = foldr (++) "" [" " | x <- [1..n]]
getElemPart n (TextCell text) = if length text <= n 
	then text ++ (foldr (++) "" [" " | x <- [1..(n - length text)]])
	else (take (n-3) text) ++ "..."
getElemPart n (NumCell num) =  if length text <= n 
	then text ++ (foldr (++) "" [" " | x <- [1..(n - length text)]])
	else (take (n-3) text) ++ "..." where
		text = (show num)

displayFullElem :: [[CellContent]] -> CellCoords -> IO ()
displayFullElem sheet (Cell (col, row)) = case ((sheet !! col) !! row) of
	(TextCell text) -> putStrLn ("\"" ++ text ++ "\"")
	(NumCell num) -> putStrLn (show num)
	EmptyCell -> putStrLn "This cell is empty."

-- set command
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

-- add command
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
addCells [] (Column col) = [[EmptyCell | x <- [0..5]] | y <- [0..col]]
addCells [] (Row row) = [[EmptyCell | x <- [0..row]] | y <- [0..5]]
addCells sheet (Column col) = (insertAt sheet col column) where
	rows = length (head sheet)
	column = [EmptyCell | x <- [1..rows]]	
addCells sheet (Row row) =
	[insertAt (sheet !! i) row EmptyCell | i <- [0..((length sheet) - 1)]]

-- delete command
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