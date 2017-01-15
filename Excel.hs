-- main application
module Main where

import Commands
import System.IO
import Cells

askForCommand :: [[CellContent]] -> String -> IO ()
askForCommand sheet [] = do
	--putStrLn ""
	--putStrLn "Type a command: (\"help\" for instructions)"
	putStr ">>>>> "
	hFlush stdout
	command <- getLine
	--putStrLn ""
	askForCommand sheet command

askForCommand _ "quit" =
	putStrLn "Hope your experience was EXCELlent, see you next time!"

askForCommand sheet command = do
		newSheet <- tryExecuteCommand sheet command
		askForCommand newSheet []


main = do
	putStrLn "Hello"
	putStrLn "Type \"help\" for available commands"
	let sheet = [[EmptyCell | y <- [1..5]] | x <- [1..5]]
	askForCommand sheet []
