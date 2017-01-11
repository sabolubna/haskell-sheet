-- main application
module Main where

import Commands
import System.IO
import Cells

askForCommand :: String -> IO ()
askForCommand [] = do
	putStrLn ""
	putStrLn "Type a command: (\"help\" for instructions)"
	putStr ">>>>> "
	hFlush stdout
	command <- getLine
	putStrLn ""
	askForCommand command

askForCommand "quit" = putStrLn "Hope your experience was EXCELlent, see you next time!"

askForCommand command = do
	tryExecuteCommand command
	askForCommand []


main = askForCommand []
