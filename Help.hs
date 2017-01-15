-- file for help messages
module Help where

printHelpMsg :: [String] -> IO ()
printHelpMsg [""] = do
	putStrLn ""
	putStrLn "Available commands:"
	putStrLn ""
	printHelpMsg ["show", "new", "open", "save", "set", "clear", "add", "delete", "help", "quit"]
	putStrLn ""

printHelpMsg [command] = case command of
	"show" -> do
		putStrLn "show - displays current sheet"
		putStrLn "show XX - displays contents of a single cell, e.g. show A5"
		putStrLn "show XX:XX - displays chosen section of the sheet"
		putStrLn "  e.g. show F4:J6, show A:E, show 5:10"
		putStrLn "     NOTE: no more than 5 columns will be displayed."
	"set" -> do
		putStrLn "set XX Y - puts numerical value in a cell, e.g. set A5 -5.73"
		putStrLn "set XX \"XXX\" - puts text value in a cell, e.g. set A5 \"abc\""
		putStrLn "set XX SUM(YY:ZZ) - puts sum function in a cell, e.g. set A5 SUM(A5:B7)"
		putStrLn "set XX MEAN(YY:ZZ) - puts mean function in a cell, e.g. set A5 MEAN(A5:B7)"
		putStrLn "set XX PRODUCT(YY:ZZ) - puts product function in a cell, e.g. set A5 PRODUCT(A5:B7)"
		putStrLn "     NOTE: functions works only with numerical values."
	"new" -> do 
		putStrLn "new - creates new sheet."
		putStrLn "     WARNING: current sheet is lost."
	"open" -> do 
		putStrLn "open X - opens sheet stired in file X, e.g. open f.sht"
		putStrLn "     WARNING: current sheet is lost."
	"save" -> do 
		putStrLn "save X - saves sheet in file X, e.g. save f.sht"
	"add" -> do
		putStrLn "add X - adds empty column or row, e.g. add F, add 7"
		putStrLn "  if X is bigger that sheet's current size, multiple"
		putStrLn "  columns/rows are added"
	"clear" -> do 
		putStrLn "clear XX - clears content of XX cell, e.g. clear A1"
	"delete" -> do
		putStrLn "delete X - deletes chosen column or row, e.g. delete F, delete 7"
	"help" -> do
		putStrLn "help XXX - displays help message for chosen command, e.g. help show"
	"quit" -> do 
		putStrLn "quit - closes the application"
	_ -> do
		printHelpMsg [""]

printHelpMsg (x:xs) = do
	printHelpMsg [x]
	putStrLn ""
	printHelpMsg xs