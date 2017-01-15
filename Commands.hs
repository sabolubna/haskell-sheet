-- functions that execute user's commands
module Commands where

import Help
import Cells
import Utils

columnWidth = 10
totalWidth = 70

-- Parses and executes a command, if correct
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

-- show command section
-- Verifies command's arguments and executes command
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

-- Given a sheet and a range, displays cells
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

-- Returns a string containing sheet's first row (with letters for columns)
getFirstRow :: (CellCoords, CellCoords) -> String
getFirstRow ((Column col1), (Column col2)) = rowString where
	elems = [[getCoordsString (Column col)] ++ [" " | x <- [1..(columnWidth - length (getCoordsString (Column col)))]] ++ [" | "]| col <- [col1..col2]]
	rowString = "  | " ++ (take totalWidth (foldr (++) "" (foldr (++) [""] elems)))

-- Given a sheet, single row, starting and ending column, returns a string with this row.
getRow :: [[CellContent]] -> CellCoords -> CellCoords -> CellCoords -> String
getRow sheet (Row row) (Column x1) (Column x2) = rowString where
	elems = [getElemPart sheet columnWidth ((sheet !! col) !! row) ++ " | " | col <- [x1..x2]]
	rowString = show (row+1) ++ " | " ++ (take totalWidth (foldr (++) "" elems))

-- Given the contents of a cell returns a string truncated to chosen number of chars.
getElemPart :: [[CellContent]] -> Int -> CellContent -> String
getElemPart _ n EmptyCell = foldr (++) "" [" " | x <- [1..n]]
getElemPart _ n (TextCell text) = if length text <= n 
	then text ++ (foldr (++) "" [" " | x <- [1..(n - length text)]])
	else (take (n-3) text) ++ "..."
getElemPart _ n (NumCell num) =  if length text <= n 
	then text ++ (foldr (++) "" [" " | x <- [1..(n - length text)]])
	else (take (n-3) text) ++ "..." where
		text = (show num)
getElemPart sheet n (FunctionCell (fun, range)) = if length text <= n 
	then text ++ (foldr (++) "" [" " | x <- [1..(n - length text)]])
	else (take (n-3) text) ++ "..." where
		(success, value) = calculateFunction sheet (FunctionCell (fun, range))
		text = if success then show value else "#####"

-- Displays full content of the cell.
displayFullElem :: [[CellContent]] -> CellCoords -> IO ()
displayFullElem sheet (Cell (col, row)) = case ((sheet !! col) !! row) of
	(TextCell text) -> putStrLn ("\"" ++ text ++ "\"")
	(NumCell num) -> putStrLn (show num)
	(FunctionCell (fun, range)) -> do
		putStrLn (functionToString (FunctionCell (fun, range)))
		putStrLn result where
			(success, value) = calculateFunction sheet (FunctionCell (fun, range))
			result = if success then show value else "#####"
	EmptyCell -> putStrLn "This cell is empty."

-- Calculates value of a function, returns it with a Bool whether all 
-- cells used for calculations were NumCells or EmptyCells
calculateFunction :: [[CellContent]] -> CellContent -> (Bool, Double)
calculateFunction sheet (FunctionCell (fun, Range (Cell(c1, r1), Cell(c2, r2)))) = do
	if r1 == r2 
		then (allOk, rowValue)
		else (allOk && restOk, fullValue) where
			allOk = allTrue [isNum ((sheet !! c) !! r1) | c <- [c1..c2]]
			nums = [getNum ((sheet !! c) !! r1)| c <- [c1..c2]]
			rowValue = case fun of
				Sum -> foldr (+) 0.0 nums
				Product -> foldr (*) 1.0 nums
				Mean -> foldr (+) 0.0 nums
			(restOk, restValue) = if fun /= Mean 
				then calculateFunction sheet (FunctionCell (fun, Range (Cell(c1,r1+1), Cell(c2, r2))))
				else calculateFunction sheet (FunctionCell (Sum, Range (Cell(c1,r1+1), Cell(c2, r2))))
			fullValue = case fun of
				Sum -> rowValue + restValue
				Product -> rowValue * restValue
				Mean -> (rowValue + restValue) / fromIntegral ((c2 - c1 + 1)*(r2 - r1 + 1))
			isNum :: CellContent -> Bool
			isNum (NumCell _) = True
			isNum EmptyCell = True
			isNum _ = False
			getNum :: CellContent -> Double
			getNum (NumCell num) = num
			getNum _ = 0.0

-- set command section
-- Verifies arguments and executes set command
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

-- add command section
-- Verifies arguments and executes add command
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

-- Given a sheet, adds empty columns or rows, as chosen;
-- if the sheet is empty, added columns/rows are 5-cells wide.
addCells :: [[CellContent]] -> CellCoords -> [[CellContent]]
addCells [] (Column col) = [[EmptyCell | x <- [0..5]] | y <- [0..col]]
addCells [] (Row row) = [[EmptyCell | x <- [0..row]] | y <- [0..5]]
addCells sheet (Column col) = (insertAt sheet col column) where
	rows = length (head sheet)
	column = [EmptyCell | x <- [1..rows]]	
addCells sheet (Row row) =
	[insertAt (sheet !! i) row EmptyCell | i <- [0..((length sheet) - 1)]]

-- delete command section
-- Verifies arguments and executes delete command.
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

-- Deletes chosen rows/columns from the sheet;
-- this command results in a change of the sheet size.
deleteCells :: [[CellContent]] -> CellCoords -> [[CellContent]]
deleteCells sheet (Column col) = removeAt sheet col
deleteCells sheet (Row row) = if head newSheet == []
	then [] 
	else newSheet where 
		newSheet = [removeAt (sheet !! col) row | col <- [0..(colCount-1)]]
		colCount = length sheet