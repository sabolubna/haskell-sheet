-- file for help messages
module Help where

printHelpMsg :: [String] -> IO ()
printHelpMsg [""] = do
	putStrLn "Available commands:"
	putStrLn ""
	printHelpMsg ["show", "set", "help", "quit"]

printHelpMsg [command] = case command of
	"show" -> do
		putStrLn "show - displays current sheet"
		putStrLn "show XX - displays contents of a single cell, e.g. show A5"
		putStrLn "show XX:XX - displays chosen section of the sheet"
		putStrLn "  e.g. show F4:J6, show A:E, show 5:10"
	"set" -> do
		putStrLn "set XX \"XXX\" - puts text value in a cell, e.g. set A5 \"abc\""
	"help" -> do
		putStrLn "help XXX - displays help message for chosen command, e.g. help show"
	"quit" -> do 
		putStrLn "quit - closes the application"
	_ -> do
		putStrLn "Incorrect help command"

printHelpMsg (x:xs) = do
	printHelpMsg [x]
	putStrLn ""
	printHelpMsg xs