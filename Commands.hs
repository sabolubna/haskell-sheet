-- functions that execute user's commands
module Commands where

import Help
import Cells
import Utils

tryExecuteCommand :: String -> IO ()
tryExecuteCommand command = (case head splitCommand of
		"show" -> display (tail splitCommand)
		"help" -> printHelpMsg (tail splitCommand)
		_ -> putStrLn ("Command: " ++ command ++ " is incorrect.")) where 
				splitCommand = split ' ' command


display :: [String] -> IO ()
display [] = printSheet (EmptyCoords, EmptyCoords)
display [coords] = printSheet (getCellRange coords)
display _ = putStrLn "Incorrect show command. Type help show for correct examples."


printSheet :: (CellCoords, CellCoords) -> IO ()
printSheet (EmptyCoords, EmptyCoords) = putStrLn "Showing full sheet"
printSheet (Cell (x1, y1), Cell (x2, y2)) = if x1 > x2 || y1 > y2 then
 	putStrLn "Incorrect show arguments."
	else putStrLn ("Showing " ++ show x1 ++ show y1 ++ ":" ++ show x2 ++ show y2)
printSheet (Column col1, Column col2) = if col1 > col2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show col1 ++ ":" ++ show col2)
printSheet (Row row1, Row row2) = if row1 > row2
	then putStrLn "Incorrect show arguments."
	else putStrLn ("Showing columns" ++ show row1 ++ ":" ++ show row1)
printSheet _ = putStrLn "Incorrect show arguments."